#' Highlight R Code with Custom Styles
#'
#' This function offers syntax highlighting for R code, transforming plain script into visually appealing HTML-formatted code blocks with customizable color palettes.
#' It supports highlighting different elements of R syntax, such as functions, operators, numbers, comments, and others.
#' The returned string can be directly rendered in an HTML environment.
#'
#' @param code A character string representing the R code to be highlighted.
#'        Inside the code, the inner quotes should be escaped with backslashes to prevent them from prematurely ending the string in R (see example).
#' @param style An integer indicating the highlighting style, with options 1, 2, or 3. Default is `1` (recommend).
#'        Different styles may apply variations in background frame, font, underline, or other
#'        decorations (future expansion).
#' @param color_backgroud A string specifying the background color of the code block.
#'        Default is `"#111111"` (dark background).
#' @param color_function A string specifying the color for function names.
#'        Default is `"#B294BB"` (purple shade).
#' @param color_operator A string specifying the color for operators (`+`, `-`, etc.).
#'        Default is `"#8ABEB7"` (cyan shade).
#' @param color_number A string specifying the color for numeric literals.
#'        Default is `"#DE935F"` (orange shade).
#' @param color_quotation A string specifying the color for quoted strings.
#'        Default is `"#F0C674"` (yellow shade).
#' @param color_comments A string specifying the color for comments (lines starting with `#`).
#'        Default is `"#969896"` (gray shade).
#' @param color_others A string specifying the default color for all other elements.
#'        Default is `"#FFFFFF"` (white).
#'
#' @return A character string with HTML-style span elements for syntax highlighting.
#'         The returned string can be directly rendered in an HTML environment.
#'
#' @examples
#' code <- "x <- 11 # Assign value to x
#' y <- \"eleven\" # Assign character to y"
#' cat(Rcode2html(code))
#' cat(Rcode2html(code, color_backgroud = "#323232",color_operator = "#00BFFF",
#'                color_number="#A52A2A", color_quotation = "#CDAD00", color_comments="#6495ED"))
#'
#'
#' code <- "# use package car
#' library(car)
#' m <- colMeans(x)
#' S <- cov(x)
#' plot(x[,1], x[,2], pch=19, xlim=c(-2.5,9.5), ylim=c(-2.5,9.5))
#' car::ellipse(m, S, 3.03, col=\"brown\", center.cex=1, lwd=3)"
#' cat(Rcode2html(code))
#' cat(Rcode2html(code, color_function = "#FF0000"))
#'
#' @export
Rcode2html <- function(code,
                       style = 1,
                       color_backgroud = "#111111",
                       color_function = "#B294BB",
                       color_operator = "#8ABEB7",
                       color_number = "#DE935F",
                       color_quotation = "#F0C674",
                       color_comments = "#969896",
                       color_others = "#FFFFFF") {
  # if (style %in% c(1, 2, 3)) {
  #   color_backgroud = "#111111"
  #   color_function = "#B294BB"
  #   color_operator = "#8ABEB7"
  #   color_number = "#DE935F"
  #   color_quotation = "#F0C674"
  #   color_comments = "#969896"
  #   color_others = "#FFFFFF"
  # }

  # split into many elements

  matches <- gregexpr(
    "#[^\n]*|\"[^\"]*\"|'[^']*'|\\n|\\b\\d+(\\.\\d+)?([eE][-+]?\\d+)?\\b|[a-zA-Z][a-zA-Z0-9_.]*|\\W",
    code,
    perl = TRUE
  )

  elements <- regmatches(code, matches)[[1]]

  # Highlight elements one by one
  n <- length(elements)
  res <- elements
  for (el_idx in 1:n) {
    res[el_idx] <- highlight(
      el_idx,
      elements,
      color_function,
      color_operator,
      color_number,
      color_quotation,
      color_comments,
      color_others
    )
  }

  # combine
  code2 <- paste(res, collapse = "")

  if (style == 1) {
    paste0(
      "<!-- HTML generated using R package Rcode2html -->\n<div style=\"padding: 14px 14px 16px; border: 1px solid ",
      color_backgroud,
      "; border-radius: 7px; background-color: ",
      color_backgroud,
      " !important; text-align: justify; font-family: Consolas, 'Courier New', monospace;\">\n<pre  style=\"margin: 0; border: inherit; font-family: inherit; line-height: 1.25; color: inherit; background-color: inherit;\">\n",
      code2,
      "\n</pre>\n</div>\n"
    )

  } else if (style == 2) {
    paste0(
      "<!-- HTML generated using R package Rcode2html -->\n<div style=\"background:",
      color_backgroud,
      "; overflow:auto;width:auto;border:solid gray;border-width:.1em .1em .1em .8em;padding:.2em .6em; font-family: Consolas, 'Courier New', monospace;\">\n<pre style=\"margin: 0; border: 1px solid ",
      color_backgroud,
      "; font-family: inherit; line-height: 1.25; color: inherit; background-color: inherit;\">\n",
      code2,
      "\n</pre>\n</div>\n"
    )
  } else if (style == 3) {
    paste0(
      "<!-- HTML generated using R package Rcode2html -->\n<div style=\"padding: 14px 14px 16px; border: 1px solid ",
      color_backgroud,
      "; border-radius: 7px; background-color: ",
      color_backgroud,
      " !important; text-align: justify;\">\n<pre style=\"margin: 0; border: inherit; font-family: inherit; line-height: 1.25; color: inherit; background-color: inherit;\">\n",
      code2,
      "\n</pre>\n</div>\n"
    )
  }
}

# highlight elements
highlight <- function(el_idx,
                      elements,
                      color_function,
                      color_operator,
                      color_number,
                      color_quotation,
                      color_comments,
                      color_others) {
  el <- elements[el_idx]
  if (grepl("^\\s*$", el)) {
    # Spaces and empty strings. no need to process
    return(el)
  } else if (grepl("^[a-zA-Z][a-zA-Z0-9_.]*$|^[.][a-zA-Z_.]*$", el) &
             (el_idx < length(elements) &
              elements[el_idx + 1] == "(")) {
    # Function name check: If the element is followed by "(", it is regarded as a function
    return(
      sprintf(
        "<span style=\"color: %s; font-weight: bold\">%s</span>",
        color_function,
        el
      )
    )
  } else if (grepl(
    "^(\\+|-|\\*|/|\\^|%%|%/%|<|>|<=|>=|==|!=|&|\\||!|&&|\\|\\||<-|->|<<-|->>|=|~|:|::|:::|\\$|@|\\[\\[|\\]|\\[|\\(|\\)|%[^%]*%)$",
    el
  )) {
    # operators
    return(sprintf("<span style=\"color: %s\">%s</span>", color_operator, el))
  } else if (grepl("^\\d+(\\.\\d+)?$", el)) {
    # numbers
    return(sprintf("<span style=\"color: %s\">%s</span>", color_number, el))
  } else if (grepl("^(\"[^\"]*\"|'[^']*')$", el)) {
    # quotation
    return(sprintf("<span style=\"color: %s\">%s</span>", color_quotation, el))
  } else if (grepl("^#", el)) {
    # notes
    return(sprintf("<span style=\"color: %s\">%s</span>", color_comments, el))
  } else {
    # others
    return(sprintf("<span style=\"color: %s\">%s</span>", color_others, el))
  }
}
