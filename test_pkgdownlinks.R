rm(list = ls())

url_validator <- Vectorize(function(testurl) {
  connection <- url(testurl)
  check <- suppressWarnings(try(open.connection(connection, open = "rt", timeout = 2), silent = TRUE)[1])
  suppressWarnings(try(close.connection(connection), silent = TRUE))

  return(is.null(check))
}, vectorize.args = "testurl")


library(xml2)
library(rvest)

search_hrefs <- Vectorize(function(testurl) {
  htmlpage <- xml2::read_html(testurl)
  uphrefs <- rvest::html_attr(rvest::html_nodes(htmlpage, "a"), "href")

  ind_rm <- unique(c(grep("../", uphrefs), grep("#", uphrefs)))
  uphrefs <- uphrefs[-ind_rm]

  return(uphrefs)
}, vectorize.args = "testurl")

referenceurl <- "https://www.math.uzh.ch/pages/spam-devel/reference/index.html"

referencesrefs <- paste0("https://www.math.uzh.ch/pages/spam-devel/reference/", search_hrefs(referenceurl))
referenceslinks <- search_hrefs(referencesrefs)

tests <- lapply(referenceslinks, function(x) { url_validator(paste0("https://www.math.uzh.ch/pages/spam-devel/reference/", x)) })

