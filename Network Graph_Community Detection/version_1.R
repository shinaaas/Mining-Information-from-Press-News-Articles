library(shiny)
library(dplyr)
library(tidytext)
library(igraph)
library(tidyr)
library(glue)
library(networkD3)
library(purrr)
library(ggplot2)
library(cowplot)
library(readr)
library(stringr)
library(widyr)
library(readxl)

#function to replace spaces in multi-word entities with underscores
preprocess_text <- function(text, entities) {
  for (entity in entities) {
    entity_underscore <- str_replace_all(entity, " ", "_")
    text <- str_replace_all(text, regex(entity, ignore_case = TRUE), entity_underscore)
  }
  return(text)
}

#function to extract sentences containing named entities
extract_sentences_with_entities <- function(text, entities) {
  sentences <- unnest_tokens(tibble(text = text), sentence, text, token = "sentences")
  sentences <- sentences %>%
    filter(str_detect(sentence, paste(entities, collapse = "|")))
  return(sentences)
}

#function to extract co-occurrences and remove duplicates
extract_cooccurrences <- function(sentences, entities) {
  cooccurrences <- sentences %>%
    mutate(sentence_id = row_number()) %>%  
    unnest_tokens(word, sentence) %>%  #tokenize the sentences into words
    filter(word %in% entities) %>%  #filter only the named entities
    pairwise_count(word, sentence_id, sort = TRUE) %>%  #calculate co-occurrences
    mutate(pair = pmap_chr(list(item1, item2), ~paste(sort(c(.x, .y)), collapse = ","))) %>% 
    group_by(pair) %>%
    summarize(n = sum(n)) %>% #aggregate the counts for each standardized pair
    separate(pair, into = c("item1", "item2"), sep = ",")
  
  return(cooccurrences)
}

#UI
ui <- fluidPage(
  titlePanel("Network Analysis of News Articles"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV or Excel File with Article Text",
                accept = c("text/csv", ".csv", ".xls", ".xlsx")),
      helpText("The article text should be in the first column."),
      fileInput("entities_file", "Choose Excel File with Named Entities",
                accept = c(".xls", ".xlsx")),
      helpText("The list of named entities to be used as nodes should be in the first column."),
      numericInput("threshold", "Threshold", value = 10, min = 1),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network Community", forceNetworkOutput("networkPlot")),
        tabPanel("List of Communities", verbatimTextOutput("communities"))
      )
    )
  )
)

#server
server <- function(input, output) {
  #increase the maximum request size to 50MB
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  observeEvent(input$analyze, {
    req(input$file1)
    req(input$entities_file)
    
    #read the uploaded file
    if (str_detect(input$file1$name, "\\.csv$")) {
      article_data <- read_csv(input$file1$datapath)
    } else {
      article_data <- read_excel(input$file1$datapath)
    }
    
    #sse the first column as article text
    ukraine_data <- article_data %>%
      select(article_text = 1) %>%
      mutate(article_text = tolower(article_text))
    
    #read the named entities from the Excel file
    named_entities_df <- read_excel(input$entities_file$datapath)
    named_entities <- tolower(named_entities_df[[1]])
    
    #preprocess the article_text column
    ukraine_data <- ukraine_data %>%
      mutate(article_text = preprocess_text(article_text, named_entities))
    
    #update named entities list to match preprocessed format
    named_entities <- str_replace_all(named_entities, " ", "_")
    
    #extract sentences with named entities
    sentences <- extract_sentences_with_entities(ukraine_data$article_text, named_entities)
    
    #extract co-occurrences
    cooccurrences <- extract_cooccurrences(sentences, named_entities)
    
    #replace underscores back with spaces in co-occurrences
    cooccurrences <- cooccurrences %>%
      mutate(across(c(item1, item2), ~str_replace_all(., "_", " ")))
    
    #create the network graph
    network <- cooccurrences %>%
      rename(weight = n) %>%
      filter(weight > input$threshold) %>%
      graph_from_data_frame(directed = FALSE)
    
    density <- edge_density(network)
    print(paste("Network Density:", density))
    
    #store the degree
    V(network)$degree <- strength(graph = network)
    
    #compute the weight shares
    E(network)$width <- E(network)$weight / max(E(network)$weight)
    
    #create networkD3 object
    network_D3 <- igraph_to_networkD3(g = network)
    network_D3$nodes <- network_D3$nodes %>%
      mutate(name = str_replace_all(name, "_", " "),  #replace underscores back with spaces
             Degree = (1E-2) * V(network)$degree,
             Group = 1)
    network_D3$links$Width <- 10 * E(network)$width
    
    #output the network plot
    output$networkPlot <- renderForceNetwork({
      forceNetwork(
        Links = network_D3$links, 
        Nodes = network_D3$nodes, 
        Source = 'source', 
        Target = 'target',
        NodeID = 'name',
        Group = 'Group', 
        opacity = 0.9,
        Value = 'Width',
        Nodesize = 'Degree', 
        linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
        fontSize = 15,
        zoom = TRUE, 
        opacityNoHover = 1,
        charge = -500,
        linkDistance = 200
      )
    })
    
    #community detection using Louvain method
    comm_det_obj <- cluster_louvain(graph = network, weights = E(network)$weight)
    
    #encode the membership as a node attribute
    V(network)$membership <- membership(comm_det_obj)
    
    #use the membership label to color the nodes
    network_D3$nodes$Group <- V(network)$membership
    
    #output the communities
    output$communities <- renderPrint({
      membership_df <- tibble(
        word = V(network)$name,
        cluster = V(network)$membership
      )
      
      clusters <- V(network)$membership %>%
        unique() %>%
        sort() %>%
        map_chr(.f = function(cluster_id) {
          membership_df %>%
            filter(cluster == cluster_id) %>%
            slice(1:15) %>%
            pull(word) %>%
            str_c(collapse = ', ')
        })
      
      clusters
    })
  })
}

#run the application 
shinyApp(ui = ui, server = server)