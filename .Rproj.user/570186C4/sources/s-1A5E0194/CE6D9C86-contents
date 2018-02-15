##################################
### Donald Rumsfeld Snowflakes ###
##################################

# If you don't already have it, you are going to need the pdftools package, 
# in addition to an installation of Xpdf tools: http://www.xpdfreader.com/download.html.
# Without doubt, you already have tm installed...right?

library(dplyr); library(stringr); library(tm)

# Specifying our reader controls.

pdfRead = readPDF(control = list(text = "-layout"))

# Reading in the snowflake doc.

snowflakes = Corpus(URISource("data/11-L-0559-First-Release-Bates-1-912.pdf"), 
                    readerControl = list(reader = pdfRead))

# We don't have much in the way of metadata or the sort, so I just want to make
# the object name short.

snowflakes = snowflakes[["11-L-0559-First-Release-Bates-1-912.pdf"]][["content"]]

# As with any adventure in text, we need to do some heavy cleaning.

snowflakes = str_replace_all(snowflakes, "\\r|\\n|\\s{2,50}|\\t+", " ") %>%
  str_replace_all("~|\\*|<|>|•", "") %>%
  str_trim(.) %>% 
  na_if("") %>% 
  na.omit()

# I want to create a data frame, because I want columns with things like 
# dates, the recipient(s), etc.

snowflakeDF = data.frame(message = snowflakes)

snowflakeDF = snowflakeDF %>% 
  mutate(snowflakeIndicator = ifelse(grepl("snowflake", .$message), 1, 
                              ifelse(grepl("FROM: Donald", .$message), 1, "")),
    dateRaw = stringr::str_extract(message, "([A-Z][a-z]+\\s*[0-9]{1,2},*\\s*[0-9]{4})"), 
    dateMonth = stringr::str_extract(dateRaw, "([A-Za-z]+)"), 
    dateMonth = match(dateMonth, month.name),
    dateMonth = ifelse(nchar(dateMonth) == 1, paste("0", dateMonth, sep = ""), dateMonth),
    dateDay = stringr::str_extract(dateRaw, "([0-9]{1,2})"),
    dateDay = ifelse(nchar(dateDay) == 1, paste("0", dateDay, sep = ""), dateDay),
    dateYear = stringr::str_extract(dateRaw, "([0-9]{4})")) %>% 
  tidyr::unite(., dateJoin, dateMonth, dateDay, dateYear, sep = "") %>% 
  mutate(formattedDate = lubridate::mdy(dateJoin), 
         recipients = stringr::str_extract(.$message, "TO:.*FROM"), 
         recipients = stringr::str_replace_all(recipients, "TO:|FROM|cc|CC|:", "")) %>% 
  select(-dateJoin, -dateRaw)













