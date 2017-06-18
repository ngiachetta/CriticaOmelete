library(rvest)
library(tidyverse)
library(XML)
library(stringr)


###########
# Parte I #
###########

#Extraindo uma lista com todas as críticas do omelete
url_base <- "https://omelete.uol.com.br/filmes/critica/?pagina=NUMCRIT#filters"

lista_url <- c()
for (i in 1:212){
  url_critica <- gsub('NUMCRIT',i, url_base)
  lista_url <- append(lista_url, url_critica)
}

linksomelete <- function(numero_pagina) { #Funcao para extrair somente os links de cada critica
  ler_url <- read_html(lista_url[numero_pagina])
  LinkCritica <- ler_url %>% 
    html_nodes("a") %>%
    html_attr("href")
  
  LinkCritica <- as.data.frame(LinkCritica)
  colnames(LinkCritica) <- c("url")
  LinkCritica <- LinkCritica[grepl("?key", LinkCritica[["url"]]), ]
  LinkCritica <- as.character(LinkCritica)
  LinkCritica <- as.data.frame(LinkCritica)
  LinkCritica <- data.frame(lapply(LinkCritica, as.character), stringsAsFactors = FALSE)
  
  vetor_critica <- as.vector(LinkCritica[, 1])
  vetor_critica <- gsub(pattern =" " ,"", vetor_critica)
  vetor_critica <- unique(vetor_critica)
} 

lista_final <- c()
for (j in 1:length(lista_url)) {
  lista_final <- append(lista_final, linksomelete(j))
  lista_final <- unique(lista_final)
}
for (k in 1:length(lista_final)) {
  lista_final[k] <- paste("https://omelete.uol.com.br", lista_final[k], sep = "")
  print(lista_final[k])
}

############
# Parte II #
############

#Obtendo as variaveis

tituloV <- c()
dataCriticaV <- c()
horaCriticaV <- c()
conteudoV <- c()
duracaofilmeV <- c()
estreiaV <- c()
classfetariaV <- c()
paisV <- c()
autorcriticaV <- c()
notacriticaV <- c()
direcaoV <- c()
generoV <- c()

for (m in 1:length(lista_final)) {
  linkurl <- read_html(lista_final[m])
  
  #Titulo do filme
  titulo <- NULL
  titulo[m] <- linkurl %>% 
    html_node ("title") %>% 
    html_text ()
  
  titulo[m] <- gsub("| Crítica | Omelete", "", titulo[m]) #Substituindo valor, porém nesse caso gera "||" que é retirado abaixo
  titulo[m] <- gsub("[||]", "", titulo[m])
  titulo[m] <- str_trim(titulo[m])
  ##Criando o vetor
  tituloV <- append(tituloV, titulo[m])
  
  #Data e hora em que foi postado a critica
  dataHora <- NULL
  dataCritica <- NULL 
  horaCritica <- NULL
  
  dataHora[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[2]/div[1]/div/div[1]/div/div[4]/div[2]/span[1]') %>% 
    html_text()
  
  dataHora[m] <- gsub("[[:space:]]", "", dataHora[m]) #Retira todos os espaços, inclusive entre as palavras
  
  dataCritica[m] <- str_sub(dataHora[m], start = 1, end = 10)
  #dataCritica[m] <- dmy(dataCritica[m])
  
  horaCritica[m] <- str_sub(dataHora[m], start = 12, end = 16)
  ##Criando o vetor
  dataCriticaV <- append(dataCriticaV, dataCritica[m])
  horaCriticaV <- append(horaCriticaV, horaCritica[m])
  
  #Conteudo da critica
  conteudo <- NULL
  conteudo[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[2]/div[1]/div/div[1]/div/div[5]') %>% 
    html_text()
  
  conteudo[m] <- gsub("\n", " ", str_trim(conteudo[m], "left")) #Retira \n e o espaço a esquerda
  ##Criando o vetor
  conteudoV <- append(conteudoV, conteudo[m])
  
  #Duracao do filme(em minutos)
  duracaofilme <- NULL
  duracaofilme[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[2]/ul/li[4]/span[2]') %>% 
    html_text()
  duracaofilme[m] <- str_trim(gsub("min.", "", duracaofilme[m]))
  ##Criando o vetor
  duracaofilmeV <- append(duracaofilmeV, duracaofilme[m])
  
  #Estreia do filme
  estreia <- NULL
  estreia[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[2]/ul/li[3]/span[2]') %>% 
    html_text()
  ##Criando o vetor
  estreiaV <- append(estreiaV, estreia[m])
  
  #Classificacao etaria
  classfetaria <- NULL
  classfetaria[m] <-  linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[2]/ul/li[2]/span[2]') %>% 
    html_text()
  ##Criando o vetor
  classfetariaV <- append(classfetariaV, classfetaria[m])
  
  #Pais de gravação do filme
  pais <- NULL
  pais[m] <- linkurl %>% 
    html_nodes(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[2]/ul/li[1]/span[2]') %>%
    html_text()
  ##Criando o vetor
  paisV <- append(paisV, pais[m])
  
  #Autor da critica
  autorcritica <- NULL
  autorcritica[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[2]/div[1]/div/div[1]/div/div[4]/div[2]/span[2]/span[1]') %>% 
    html_text()
  
  autorcritica[m] <- str_trim(autorcritica[m])
  ##Criando o vetor
  autorcriticaV <- append(autorcriticaV, autorcritica[m])
  
  #Nota da critica
  notacritica <- NULL
  notacritica[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[2]/div[1]/div/div[1]/div/div[6]/span[3]') %>% 
    html_text()
  notacritica[m] <- gsub("\\(", "", gsub("\\)", "", notacritica[m]))
  ##Criando o vetor
  notacriticaV <- append(notacriticaV, notacritica[m])
  
  #Direcao do filme
  direcao <- NULL
  direcao[m] <- linkurl %>% 
    html_node(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[3]/ul/li[1]/span[2]/a') %>% 
    html_text()

  ##Criando o vetor
  direcaoV <- append(direcaoV, direcao[m])
  
  #Genero do filme
  genero <- NULL
  genero[m] <- linkurl %>% html_nodes(xpath = '//*[@id="conteudo"]/div[2]/div/div[1]/div/div/div[2]/div/div[1]/div[1]/span[2]') %>%
    html_text()
  
  if(length(genero[m]) == 0){ #Caso ele retorne um valor vazio, transformar em NA
    genero[m] <- "Indefinido"
  }
  #Criando o vetor
  generoV <- append(generoV, genero[m])
}

#############
# Parte III #
#############

# Criando o data_frame

OmeleteDf <- data_frame(tituloV, conteudoV, duracaofilmeV, estreiaV, classfetariaV,
                        paisV, direcaoV, generoV, autorcriticaV, notacriticaV, horaCriticaV,
                        dataCriticaV)
OmeleteDf <- unique(OmeleteDf)

############
# Parte IV #
############

# Arrumando o banco de dados

str(OmeleteDf)

# Arrumando as variáveis

## Duracao do Filme
# No site do Omelete alguns filmes tem duração em horas (separados por um h)
# Além disso, algumas observações vieram com o final escrito 'tos'

OmeleteDf$duracaofilmeV <- gsub("tos", "", str_trim(OmeleteDf$duracaofilmeV))

duracaoErrada <- str_detect(OmeleteDf$duracaofilmeV, "h") 
idErrado <- c() #Vetor com as horas no formato errado.
for (i in 1:length(duracaoErrada)) {
  if(duracaoErrada[i] == TRUE){
    idErrado <- append(idErrado, i)
  }
}

# Colocando os valores com "h" em minutos ##Arrumar depois
for (i in 1:length(idErrado)) {
  valor <- idErrado[i]
  if(str_sub(OmeleteDf$duracaofilmeV[valor],start = 2, end = 2) == "h") {
    val1 <- str_sub(OmeleteDf$duracaofilmeV[valor], end = 1)
    val2 <- str_sub(OmeleteDf$duracaofilmeV[valor], start = 3)
    val1 <- as.integer(val1)
    val2<- as.integer(val2)
    hora <- (val1*60)+val2
    hora <- as.character(hora)
    print(hora)
    OmeleteDf$duracaofilmeV[valor] <- hora
  }else {
    val1 <- str_sub(OmeleteDf$duracaofilmeV[valor],start = 1, end = 2)
    val2 <- str_sub(OmeleteDf$duracaofilmeV[valor],start = 4, end = 5)
    val1 <- as.integer(val1)
    val2<- as.integer(val2)
    hora <- (val1*60)+val2
    hora <- as.character(hora)
    print(hora)
    OmeleteDf$duracaofilmeV[valor] <- hora
  }
}
OmeleteDf$duracaofilmeV <- str_trim(OmeleteDf$duracaofilmeV)
# Transformar a variável duracaofilmeV em numérico

OmeleteDf$duracaofilmeV <- as.numeric(OmeleteDf$duracaofilmeV) #Na são os valores ausentes no site, dados como indisponiveis

#Plotando um exemplo
p1 <- OmeleteDf %>% 
  filter(notacriticaV != "críticas de Filmes" & duracaofilmeV >=0) %>% 
  ggplot(data = ., aes(x =  as.factor(notacriticaV), y =duracaofilmeV))+
  geom_boxplot()
p1
# Criar uma variável númerica que representa a notacritica

OmeleteDf$notacriticaV <- str_trim(OmeleteDf$notacriticaV)
OmeleteDf <- OmeleteDf %>% 
  mutate(notanumerica = notacriticaV, 
         notanumerica = recode(notacriticaV,
                               "críticas de Filmes" = NaN,
                               "Ruim" = 1,
                               "Regular" = 2,
                               "Bom" = 3,
                               "Ótimo" = 4,
                               "Excelente" = 5))

write.csv2(OmeleteDf, "OmeleteData.csv", sep = ";")

