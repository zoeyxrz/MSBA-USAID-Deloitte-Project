library(dplyr)
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2)
invisible(lapply(
  file.path(
    "https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions", 
    c("topicmodels2LDAvis.R", "optimal_k.R")
  ),
  devtools::source_url
))

stops <- c(
  tm::stopwords("english"),
  tm::stopwords("SMART"),
  "governor", "president", "mister", "obama","romney"
) %>%
  gofastr::prep_stopwords() 

corpus <- Corpus(DirSource("/Users/zhaorc/Desktop/Practicum/bigtxt"))
corpus <- tm_map(corpus, stripWhitespace)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
dtm <- DocumentTermMatrix(corpus, 
                          control = list(
                            tokenize = TrigramTokenizer,
                            stemming = FALSE, 
                            stopwords = TRUE,                                          
                            removePunctuation = TRUE))
control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)
k <- optimal_k(dtm, 40, control = control)
#---------ggplot-----------#
library(ggplot2)
seqk<-seq(2,50,10)

ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))
#-------ends--------------

control[["seed"]] <- 100
lda_model <- topicmodels::LDA(dtm, k=as.numeric(k), method = "Gibbs", 
                              control = control)
topics <- topicmodels::posterior(lda_model, dtm)[["topics"]]
heatmap(topics, scale = "none")
#-----topic network-----------------
library(igraph)
post <- topicmodels::posterior(lda_model)

cor_mat <- cor(t(post[["terms"]]))
cor_mat[ cor_mat < .05 ] <- 0
diag(cor_mat) <- 0

graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
graph <- delete.edges(graph, E(graph)[ weight < 0.05])

E(graph)$edge.width <- E(graph)$weight*20
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 15

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
            edge.color = "orange", vertex.color = "orange", 
            vertex.frame.color = NA, vertex.label.color = "grey30")
title("Strength Between Topics Based On Word Probabilities", cex.main=.8)
#------ends------
class(topics)
terms_10<-terms(lda_model, 10) #gives you what is for each topic
class(terms_10)

#-------beaurifulplot-----------------------------------
topic_dat <- dplyr::add_rownames(as.data.frame(topics), "Topic")
colnames(topic_dat)[-1] <- apply(terms(lda_model, 10), 2, paste, collapse = ", ")

tidyr::gather(topic_dat, Topic, Proportion, -c(Topic)) %>%
  tidyr::separate(Person_Time, c("Person", "Time"), sep = "_") %>%
  dplyr::mutate(Person = factor(Person, 
                                levels = c("OBAMA", "ROMNEY", "LEHRER", "SCHIEFFER", "CROWLEY", "QUESTION" ))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
  ggplot2::geom_bar() +
  ggplot2::coord_flip() +
  ggplot2::guides(fill=FALSE) +
  ggplot2::xlab("Proportion")
