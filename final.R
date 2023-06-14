# 텍스트마이닝 기말과제

# 한글 깨짐
library(extrafont)
font_import()
theme_set(theme_grey(base_family='AppleGothic'))

# 데이터 불러오기
tsla <- readLines("tsla.txt", encoding = "UTF-8")
tsla

library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(KoNLP)
library(ggwordcloud)
library(tidyr)
library(textclean)

#  전처리 : 한글 제외 문자 삭제 & 공백 삭제 & tibble 화
tsla <- tsla %>% str_replace_all("[^가-힣]", " ") %>% str_squish() %>% as_tibble()

head(tsla)

# 전처리 : 토큰화 
word_space <- tsla %>% unnest_tokens(input = value, output = word, token = "words") %>% filter(str_count(word) > 1)

word_space <- word_space %>% count(word, sort = T)
word_space

top20 <- word_space %>% head(20)
ggplot(top20, aes(x = reorder(word, n), y = n, fill = reorder(word,n))) +
  geom_col() +
  coord_flip()+
  labs(x="", y="",
       title="단어빈도")

# KoNLP 로 토큰화
word_noun <- tsla %>% unnest_tokens(input = value, output = word, token = extractNoun) %>% count(word, sort = T) %>% filter(str_count(word) > 1)
word_noun

top20_noun <- word_noun %>% head(20)
top20_noun
ggplot(top20_noun, aes(x = reorder(word, n), y = n, fill = reorder(word,n))) +
  geom_col() +
  coord_flip()+
  labs(x="", y="",
       title="단어빈도")

# 구글 폰트
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()


ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA), range = c(3, 15)) +
  scale_color_gradient(low = "#66aaf2", high = "#00CAA1") +
  theme_minimal()


# 오즈비 분석
tsla_1 <- readLines("tsla_1.txt", encoding = "UTF-8")
tsla_2 <- readLines("tsla_2.txt", encoding = "UTF-8")

tsla_1 <- tsla_1 %>% as_tibble() %>% mutate(news = "economic")
tsla_2<- tsla_2 %>%  as_tibble() %>% mutate(news = "tech")

bind_news <- bind_rows(tsla_1, tsla_2) %>% select(news, value)

news <- bind_news %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value)) %>% unnest_tokens(input = value, output = word, token = extractNoun)
news
          
freq <- news %>% count(news, word) %>% filter(str_count(word) > 1)    
freq

top10_news <- freq %>% group_by(news) %>% slice_max(n, n=10, with_ties = F)
top10_news

freq_wide <- freq %>% pivot_wider(names_from = news, values_from = n, values_fill = list(n=0))
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_eco = ((economic+1)/sum(economic+1)), ratio_tech = ((tech+1)/sum(tech+1)))
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_eco/ratio_tech)
freq_wide %>% arrange(odds_ratio)
freq_wide %>% arrange(1-odds_ratio)


# 토큰화 & 의미망 분석
library(widyr)
library(widyr)

words <- tsla %>% unnest_tokens(input = value, output = word, token = SimplePos22, drop = F)
words %>% select(word, value)

noun <- words %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, value)

noun %>% count(word, sort = T)

pvpa <- words %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word = str_replace(word, "/.*$", "다"))
pvpa %>% select(word, value)

pvpa %>% count(word, sort = T)

wordss <- bind_rows(noun, pvpa) %>% filter(str_count(word) >= 2)
wordss %>% select(word, value)

pair <- wordss %>% pairwise_count(item = word, feature = value, sort = T)
pair


library(tidygraph)
graph_pair <- pair %>% filter(n>=3) %>% as_tbl_graph()
graph_pair

library(ggraph)
ggraph(graph_pair) +
  geom_node_point() +
  geom_edge_link() +
  geom_node_text(aes(label = name))

library(showtext)
font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()

set.seed(1234)
ggraph(graph_pair, layout = "fr") +
  geom_edge_link(color = "pink", alpha = 0.5) +
  geom_node_point(color = "lightcoral", size = 5) +
  geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") +
  theme_graph()


graph_pair2 <- pair %>% filter(n>=3) %>% as_tbl_graph() %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_pair2

ggraph(graph_pair2, layout = "fr") +
  geom_edge_link(color = "pink", alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 15)) +
  geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") +
  theme_graph()



# lda 모델 (x)
tsla_2s <- tsla_2 %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value)) %>%
  distinct(value, keep_all = T) %>%
  filter(str_count(value, boundary("word")) >= 2)

tsla_2ss <- tsla_2s %>%
  unnest_tokens(input = value, output = word,
                token = extractNoun, drop = F) %>%
  filter(str_count(word) >1) %>%
  group_by(value) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(value, word)

count_word <- tsla_2ss %>%
  add_count(word) %>%
  filter(n <= 3) %>%
  select(-n)
count_word


stopword <- c("년간", "이후", "가운데", "하다")
count_word <- count_word %>%
  filter(!word %in% stopword)



count_word_doc <- count_word %>%
  count(value, word, sort = T)
count_word_doc

install.packages("quanteda")
library(quanteda)
library(tm)
dtm_word <- count_word_doc %>%
  cast_dfm(document = value, term = word, value = n)
dtm_word


# lda 모델 
raw_news <- read.csv("NewsResult_20230312-20230612.csv") %>% mutate(id = row_number())


news <- raw_news %>%
  mutate(news = str_replace_all(본문, "[^가-힣]", " "),
         news = str_squish(본문)) %>%
  distinct(본문, .keep_all = T) %>%
  filter(str_count(본문, boundary("word")) >= 3) %>%
  select(본문, id)

newss <- news %>%
  unnest_tokens(input = 본문, output = word,
                token = extractNoun, drop = F) %>%
  filter(str_count(word) > 1) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)

news_word <- newss %>%
  add_count(word) %>%
  select(-n)


count_word_doc <- news_word %>%
  count(id, word, sotr = T)
count_word_doc

dtm_word <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)
dtm_word

as.matrix(dtm_word[1:7, 1:7])

library(topicmodels)
lda_model <- LDA(dtm_word, k =8, method = "Gibbs",
                 control = list(seed = 1234))
lda_model


term_topic <- tidy(lda_model, matrix = "beta")
term_topic %>% count(topic)

term_topic %>% filter(term == "기술")

terms(lda_model, 10) %>%
  data.frame()


doc_topic <- tidy(lda_model, matrix="gamma")
doc_topic

lda_model@gamma

doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n=1)
doc_class$document <- as.integer(doc_class$document)
doc_class %>% arrange(document)


news_topic <- raw_news %>%
  left_join(doc_class, by = c("id" = "document"), multiple = "all")

news_topic <- news_topic %>% na.omit()
news_topic %>% select(id, topic)

top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))
top_terms

count_topic <- news_topic %>% count(topic)
count_topic


count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("TOPIC", topic))
count_topic_word


theme_set(theme_grey(base_family='AppleGothic'))

ggplot(count_topic_word, aes(x = reorder(topic_name, n),
                             y = n,
                             fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  geom_text(aes(label = term), 
            hjust = 1.04,
            col = "white",
            family = "AppleGothic") +
  scale_y_continuous(limits = c(0,920))+
  labs(x = NULL)


newss_topic <- news_topic %>%
  mutate(news = str_squish(replace_html(본문))) %>%
  arrange(-gamma)

newss_topic %>% select(gamma, news)

newss_topic %>% filter(topic == 1 & str_detect(news, '기술')) %>%
  head(50) %>%
  pull(news)


name_topic <- tibble(topic = 1:8,
                     name = c("TOPIC 1 : 미국 엔지니어 인사",
                                "TOPIC 2 : 개장 후 글로벌 상승 추세",
                                "TOPIC 3 : 스타링크 다음달 상장",
                                "TOPIC 4 : 뉴욕증시 발표",
                                "TOPIC 5 : 최대 거래 정보",
                                "TOPIC 6 : 테슬라 소프트웨어",
                                "TOPIC 7 : 플랫폼 마켓 투자",
                                "TOPIC 8 : 국내 해외 기업"))

top_term_topic_name <- term_topic %>%
  left_join(name_topic, by="topic")
top_term_topic_name

ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "토픽별 주요 단어 top 10",
       x = NULL, y = NULL) +
  theme(title = element_text(size = 14))
