library(shiny)
library(tidyverse)
library(emoji)
library(DT)
library(shinyvalidate)

packages <- available.packages() %>%
  as_tibble() %>%
  select(Package) %>%
  filter(between(str_length(Package),5,5)) %>%
  pull(Package)

check_wordle <- function(guess, word){
  guess_split <- tolower(guess) %>% strsplit("") %>% unlist()
  word_split <- tolower(word) %>% strsplit("") %>% unlist()

  correct <- ifelse(guess_split == word_split, emoji::emoji("green_square"), NA)
  present <- ifelse(map_lgl(guess_split, ~ .x %in% word_split),emoji::emoji("yellow_square"),NA)
  not_present <- ifelse(is.na(present),emoji::emoji("black_large_square"),NA)

  paste(coalesce(correct,present,not_present), collapse = "")
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Rdle"),
      p("It's Wordle, but with CRAN package names."),
      hr(),
      em("Five character package names only."),
      br(),
      em("Good luck - you're probably screwed."),
      hr(),
      textInput("guess_text",NULL),
      actionButton("submit_guess","Guess!", class = "btn-primary"),
      hr(),
      actionButton("reset","Reset", class = "btn-danger")
    ),
    mainPanel(
      DTOutput("display_wordle")
    )
  )
)

server <- function(input, output, session) {

  wordle_word <- reactiveVal(sample(packages,1))

  gv <- shinyvalidate::InputValidator$new()
  gv$add_rule("guess_text",~if(str_length(.x) != 5) "Five chars pls")

  observeEvent(
    input$reset,{
      wordle_word(sample(packages,1))
      # guesses(tibble(guess = character(), status = character()))
      }
    ,
    ignoreNULL = TRUE,
    ignoreInit = FALSE)

  guesses <- reactiveVal(tibble(guess = character(), status = character()))

  new_guess <- eventReactive(input$submit_guess,input$guess_text)

  observeEvent(new_guess(),{
    gv$enable()
    req(gv$is_valid())
    result <- check_wordle(new_guess(), wordle_word())

    bind_rows(
      guesses(),
      tibble(guess = new_guess(), status = result)
    ) %>%
    guesses()

    gv$disable()
  })

  output$display_wordle <- renderDT({
    req(new_guess())
    datatable(guesses(), escape = FALSE)
  })
}

shinyApp(ui, server)
