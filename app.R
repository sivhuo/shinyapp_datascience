library(tidyverse)
library(shiny)
library(bslib)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

sportie <- olympics %>% 
  distinct(sport) %>% 
  arrange(sport) %>% 
  pull(sport)

ui <- fluidPage( 
  theme = bs_theme(primary = "#ADD8E6", 
                   secondary = "#FFEBCD", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "spacelab"),
  
  # Application title
  titlePanel("Update the country within an olympic sport"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "sport", # to use in code
                  label = "Sport:", # how it looks in UI
                  choices = sportie, 
                  selected = "Alpine Skiing"
      ),
      selectInput(inputId = "team", # to use in code
                  label = "Team:", # how it looks in UI
                  multiple = TRUE, # can choose more than one
                  choices = NULL
      )
    ),
    # Show a bar plot of chosen distributors within chosen category
    mainPanel(
      plotOutput(outputId = "total_count")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  sport <- reactive({
    olympics %>% 
      filter(sport == input$sport)
  })
  
  observeEvent(sport(), {
    choices <- sport() %>% 
      distinct(team) %>% 
      arrange(team) %>% 
      pull(team)
    updateSelectInput(inputId = "team", choices = choices) 
  })
  
  cat_team <- reactive({
    sport() %>% 
      filter(team %in% input$team)
  })
  
  output$total_count <- renderPlot({
    req(input$team)
    cat_team() %>% 
      group_by(team) %>% 
      summarize(tot_count = sum(!is.na(medal))) %>% 
      ggplot(aes(y = fct_reorder(team, tot_count),
                 x = tot_count)) +
      geom_col() +
      labs(title = paste("Total count of medals under ", input$sport, " for each team"),
           x = "",
           y = "")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
