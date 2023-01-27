library(pacman)
p_load(tidyverse, shiny, readr, shinyWidgets, wesanderson,
       igraph, ggraph, ggplot2, ggwordcloud, wordcloud)

df_frequency <- read_csv("df_frecuency.csv")


my_interface <- fluidPage(   #layout two panels
  titlePanel("Frequency of words used in tweets about \n
             final world cup - Qatar 2022"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 50,  max = 1500, value = 90),
      sliderInput("max",
      "Maximun Number of Words:",
      min = 1, max = 500, value = 160)
    ), 
    mainPanel(
      plotOutput("my_graph")
    )
  )
)

my_server <- function(input, output) {
  
  freq_reactive <- reactive({input$freq})
  max_reactive <- reactive({input$max})
  
  
  output$my_graph <- renderPlot({
    set.seed(32)
    wordcloud(df_frequency$words, df_frequency$frec, scale=c(6,0.2),
                  min.freq = freq_reactive(), max.words= max_reactive(),
                  color=wes_palette("FantasticFox1"))
    
  })
  
}


shinyApp(ui = my_interface, server = my_server)
