# 필요한 패키지 로드
library(shiny)
library(ggplot2)
library(dplyr)
library(gt)
library(shinydashboard)

# UI 정의
ui <- dashboardPage(
  dashboardHeader(title = "R 시각화 어드벤처"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("레벨 1: 기본 그래프", tabName = "level1", icon = icon("chart-line")),
      menuItem("레벨 2: 데이터 필터링", tabName = "level2", icon = icon("filter")),
      menuItem("레벨 3: 그룹별 시각화", tabName = "level3", icon = icon("bar-chart")),
      menuItem("레벨 4: 시계열 시각화", tabName = "level4", icon = icon("line-chart")),
      menuItem("힌트 및 도움말", tabName = "hints", icon = icon("info-circle")),
      menuItem("데이터셋 보기", tabName = "datasets", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "level1",
              fluidRow(
                box(title = "문제 설명", width = 12,
                    "iris 데이터셋을 사용하여 Sepal.Length와 Sepal.Width의 산점도를 그리세요."
                ),
                box(title = "코드 입력", width = 12,
                    textAreaInput("code_level1", "R 코드:", rows = 10, width = "100%"),
                    actionButton("run_code_level1", "코드 실행"),
                    actionButton("show_answer_level1", "정답 보기")
                ),
                box(title = "결과 출력", width = 12,
                    plotOutput("plot_level1"),
                    verbatimTextOutput("error_level1")
                ),
                box(title = "정답 코드", width = 12, verbatimTextOutput("answer_level1"))
              )
      ),
      tabItem(tabName = "level2",
              fluidRow(
                box(title = "문제 설명", width = 12,
                    "mtcars 데이터셋을 사용하여 mpg가 20 이상인 데이터를 필터링하고 히스토그램을 그리세요."
                ),
                box(title = "코드 입력", width = 12,
                    textAreaInput("code_level2", "R 코드:", rows = 10, width = "100%"),
                    actionButton("run_code_level2", "코드 실행"),
                    actionButton("show_answer_level2", "정답 보기")
                ),
                box(title = "결과 출력", width = 12,
                    plotOutput("plot_level2"),
                    verbatimTextOutput("error_level2")
                ),
                box(title = "정답 코드", width = 12, verbatimTextOutput("answer_level2"))
              )
      ),
      tabItem(tabName = "level3",
              fluidRow(
                box(title = "문제 설명", width = 12,
                    "diamonds 데이터셋을 사용하여 cut별 price의 박스플롯을 그리세요."
                ),
                box(title = "코드 입력", width = 12,
                    textAreaInput("code_level3", "R 코드:", rows = 10, width = "100%"),
                    actionButton("run_code_level3", "코드 실행"),
                    actionButton("show_answer_level3", "정답 보기")
                ),
                box(title = "결과 출력", width = 12,
                    plotOutput("plot_level3"),
                    verbatimTextOutput("error_level3")
                ),
                box(title = "정답 코드", width = 12, verbatimTextOutput("answer_level3"))
              )
      ),
      tabItem(tabName = "level4",
              fluidRow(
                box(title = "문제 설명", width = 12,
                    "economics 데이터셋을 사용하여 시간에 따른 unemploy 변수의 변화를 시각화하세요."
                ),
                box(title = "코드 입력", width = 12,
                    textAreaInput("code_level4", "R 코드:", rows = 10, width = "100%"),
                    actionButton("run_code_level4", "코드 실행"),
                    actionButton("show_answer_level4", "정답 보기")
                ),
                box(title = "결과 출력", width = 12,
                    plotOutput("plot_level4"),
                    verbatimTextOutput("error_level4")
                ),
                box(title = "정답 코드", width = 12, verbatimTextOutput("answer_level4"))
              )
      ),
      tabItem(tabName = "hints",
              fluidRow(
                box(title = "힌트 및 도움말", width = 12,
                    h3("기본 힌트"),
                    p("ggplot2의 기본 문법은 다음과 같습니다:"),
                    pre("ggplot(data = <DATA>, aes(x = <X>, y = <Y>)) + <GEOM_FUNCTION>()"),
                    p("dplyr의 파이프 연산자는 `%>%`입니다. 이를 사용하여 데이터를 체인처럼 연결할 수 있습니다."),
                    pre("<DATA> %>% <FUNCTION1>() %>% <FUNCTION2>()"),
                    h3("레벨별 힌트"),
                    h4("레벨 1: 기본 그래프"),
                    p("산점도를 그리기 위해서는 geom_point()를 사용하세요."),
                    h4("레벨 2: 데이터 필터링"),
                    p("dplyr의 filter() 함수를 사용하여 mpg가 20 이상인 데이터를 필터링하세요."),
                    h4("레벨 3: 그룹별 시각화"),
                    p("박스플롯을 그리기 위해서는 geom_boxplot()을 사용하세요."),
                    h4("레벨 4: 시계열 시각화"),
                    p("시계열 데이터를 시각화하기 위해서는 geom_line()을 사용하세요.")
                )
              )
      ),
      tabItem(tabName = "datasets",
              fluidRow(
                box(title = "데이터셋 보기", width = 12,
                    h3("데이터셋 목록"),
                    actionButton("view_iris", "iris 데이터셋 보기"),
                    actionButton("view_mtcars", "mtcars 데이터셋 보기"),
                    actionButton("view_diamonds", "diamonds 데이터셋 보기"),
                    actionButton("view_economics", "economics 데이터셋 보기"),
                    tableOutput("dataset_table")
                )
              )
      )
    )
  )
)

# 서버 로직 정의
server <- function(input, output) {
  # 레벨 1: 기본 그래프
  observeEvent(input$run_code_level1, {
    code <- input$code_level1
    result <- tryCatch({
      eval(parse(text = code), envir = globalenv())
    }, error = function(e) {
      return(e$message)
    })
    
    if (inherits(result, "ggplot")) {
      output$plot_level1 <- renderPlot({ result })
      output$error_level1 <- renderText({ "" })
    } else {
      output$plot_level1 <- renderPlot({ NULL })
      output$error_level1 <- renderText({ result })
    }
  })
  
  # 정답 보기: 레벨 1
  observeEvent(input$show_answer_level1, {
    answer_code <- "
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
"
    output$answer_level1 <- renderText({ answer_code })
  })
  
  # 레벨 2: 데이터 필터링
  observeEvent(input$run_code_level2, {
    code <- input$code_level2
    result <- tryCatch({
      eval(parse(text = code), envir = globalenv())
    }, error = function(e) {
      return(e$message)
    })
    
    if (inherits(result, "ggplot")) {
      output$plot_level2 <- renderPlot({ result })
      output$error_level2 <- renderText({ "" })
    } else {
      output$plot_level2 <- renderPlot({ NULL })
      output$error_level2 <- renderText({ result })
    }
  })
  
  # 정답 보기: 레벨 2
  observeEvent(input$show_answer_level2, {
    answer_code <- "
mtcars %>%
  filter(mpg >= 20) %>%
  ggplot(aes(x = mpg)) +
    geom_histogram(binwidth = 1)
"
    output$answer_level2 <- renderText({ answer_code })
  })
  
  # 레벨 3: 그룹별 시각화
  observeEvent(input$run_code_level3, {
    code <- input$code_level3
    result <- tryCatch({
      eval(parse(text = code), envir = globalenv())
    }, error = function(e) {
      return(e$message)
    })
    
    if (inherits(result, "ggplot")) {
      output$plot_level3 <- renderPlot({ result })
      output$error_level3 <- renderText({ "" })
    } else {
      output$plot_level3 <- renderPlot({ NULL })
      output$error_level3 <- renderText({ result })
    }
  })
  
  # 정답 보기: 레벨 3
  observeEvent(input$show_answer_level3, {
    answer_code <- "
ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot()
"
    output$answer_level3 <- renderText({ answer_code })
  })
  
  # 레벨 4: 시계열 시각화
  observeEvent(input$run_code_level4, {
    code <- input$code_level4
    result <- tryCatch({
      eval(parse(text = code), envir = globalenv())
    }, error = function(e) {
      return(e$message)
    })
    
    if (inherits(result, "ggplot")) {
      output$plot_level4 <- renderPlot({ result })
      output$error_level4 <- renderText({ "" })
    } else {
      output$plot_level4 <- renderPlot({ NULL })
      output$error_level4 <- renderText({ result })
    }
  })
  
  # 정답 보기: 레벨 4
  observeEvent(input$show_answer_level4, {
    answer_code <- "
ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()
"
    output$answer_level4 <- renderText({ answer_code })
  })
  
  # 데이터셋 보기
  observeEvent(input$view_iris, {
    output$dataset_table <- renderTable({ head(iris) })
  })
  
  observeEvent(input$view_mtcars, {
    output$dataset_table <- renderTable({ head(mtcars) })
  })
  
  observeEvent(input$view_diamonds, {
    output$dataset_table <- renderTable({ head(diamonds) })
  })
  
  observeEvent(input$view_economics, {
    output$dataset_table <- renderTable({ head(economics) })
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
