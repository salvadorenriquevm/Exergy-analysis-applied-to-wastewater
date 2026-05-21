##Creating ToS - Concise Method

library(tosr)
library(tidyverse)
install.packages("bibliometrix", type = "binary", dependencies = TRUE)

library(bibliometrix)

# Data loading and creating ToS
ToS_short <- tosr::tosR("wox.txt",
                        "scopux.bib")
nrow(ToS_short)

getwd()
##Creating ToS - Comprehensive Method

# Creating the merged dataframe, graph, and nodes dataframe
tosr_files <- tosr::tosr_load("wox.txt",
                              "scopux.bib")
ToS_large <- tosr::tosSAP(graph = tosr_files$graph,
                          df = tosr_files$df,
                          nodes = tosr_files$nodes)

##Extended Scientometric Analysis - Citation Network

# Creating the environment
library(tosr)
library(tidyverse)
library(bibliometrix)
library(tidygraph)

# Data gettingtosr_files <-
tosr::tosr_load("scopux.bib",
                "wox.txt")
# Data tidyingtosr_citation_network<- 
tosr_files <- tosr::tosr_load(
  "scopux.bib",
  "wox.txt"
)

tosr_citation_network <- tosr_files$graph %>%
  tidygraph::as_tbl_graph() %>%
  tidygraph::activate(nodes) %>%
  dplyr::left_join(
    tosr_files$nodes,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::mutate(
    PY = stringr::str_extract(name, "[0-9]{4}"),
    in_degree  = tidygraph::centrality_degree(mode = "in"),
    out_degree = tidygraph::centrality_degree(mode = "out")
  )

# ================================
# Figure: Top subfields by size
# ================================
figure_subfields <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  data.frame() %>%
  dplyr::count(subfield, sort = TRUE) %>%
  dplyr::slice(1:10) %>%
  ggplot2::ggplot(
    aes(x = reorder(subfield, n), y = n)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(group = 1) +
  ggplot2::labs(
    title = "Subfields by size",
    x = "Subfields",
    y = "Papers"
  ) +
  ggplot2::theme(
    text = element_text(
      color = "black",
      face = "bold",
      family = "Times New Roman"
    ),
    plot.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


# ================================
# Data preparation: subfields over time
# ================================
tosr_graph <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::select(PY, subfield) %>%
  dplyr::filter(subfield <= 3, PY >= 2000) %>%
  tidyr::drop_na() %>%
  dplyr::count(PY, subfield, name = "n") %>%
  tidyr::pivot_wider(
    names_from  = subfield,
    values_from = n,
    values_fill = 0
  ) %>%
  tidyr::pivot_longer(
    cols      = -PY,
    names_to  = "subfield",
    values_to = "n"
  )

figure_subfields

# ================================
# Figure: Subfields production through time
# ================================
figure_subfields_time <- tosr_graph %>%
  ggplot2::ggplot(
    aes(
      x = factor(PY),
      y = n,
      group = as.character(subfield),
      color = as.character(subfield)
    )
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(
    title = "Subfields production through time",
    y = "Papers",
    x = "Years"
  ) +
  ggplot2::theme(
    legend.position = "right",
    text = element_text(
      color = "black",
      face = "bold",
      family = "Times New Roman"
    ),
    plot.title = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.text.x = element_text(
      colour = "black",
      angle = 45,
      vjust = 0.5
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )

figure_subfields_time


# ================================
# Figure: Citation network
# ================================
# ================================
# Figure: Citation network
# ================================
figure_citation_network <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::filter(subfield <= 3) %>%
  tidygraph::mutate(Subfield = as.character(subfield)) %>%
  ggraph::ggraph(layout = "graphopt") +
  ggraph::geom_edge_link(colour = "lightgray") +
  ggraph::geom_node_point(
    aes(color = Subfield, size = in_degree)
  ) +
  ggplot2::scale_size(name = "Degree") +
  ggraph::theme_graph() +
  ggplot2::labs(title = "Citation Network")

figure_citation_network
# ================================
# Combine figures
# ================================

install.packages("patchwork")
install.packages("patchwork", type = "binary")

library(ggplot2)
library(patchwork)

fig_abc <- (figure_subfields / figure_subfields_time) | 
  figure_citation_network

fig_abc + patchwork::plot_annotation(tag_levels = "a")

figure_subfields
figure_subfields_time
figure_citation_network

#ver los subfiles existentes
tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  distinct(subfield) %>%
  arrange(subfield)

#10 papers mas influyentes de cada subfile

sample_articles_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    subfield,
    name,
    PY,
    in_degree,
    out_degree
  )

table(sample_articles_subfield$subfield)

sample_articles_subfield_clean <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 30) %>%   # 🔥 más grande
  dplyr::ungroup() %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  )
sample_articles_subfield_clean <- sample_articles_subfield_clean %>%
  dplyr::mutate(
    PY_final = dplyr::coalesce(
      as.character(PY.y),
      stringr::str_extract(name, "[0-9]{4}")
    ),
    AU_final = dplyr::coalesce(AU, name),
    TI_final = TI
  )

sample_articles_subfield_clean <- sample_articles_subfield_clean %>%
  dplyr::filter(!is.na(TI_final)) %>%
  dplyr::group_by(subfield) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    subfield,
    PY = PY_final,
    TI = TI_final,
    AU = AU_final,
    in_degree,
    out_degree
  )

write.csv(
  sample_articles_subfield_clean,
  "Top_articles_by_subfield.csv",
  row.names = FALSE
)

sum(is.na(sample_articles_subfield_clean$TI))  # debe ser 0
sum(is.na(sample_articles_subfield_clean$AU))  # debe ser 0
sum(is.na(sample_articles_subfield_clean$PY))  # debe ser 0


articles_per_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::count(subfield, name = "n_articles") %>%
  dplyr::arrange(desc(n_articles))
articles_per_subfield


#articulos con metadatos
articles_per_subfield_docs <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::filter(!is.na(TI)) %>%
  dplyr::count(subfield, name = "n_articles")

articles_per_subfield_docs

table_3_articles_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 15) %>%          # tomamos más para compensar NA
  dplyr::ungroup() %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::mutate(
    PY_final = dplyr::coalesce(
      as.character(PY.y),
      stringr::str_extract(name, "[0-9]{4}")
    )
  ) %>%
  dplyr::filter(!is.na(TI)) %>%           # solo artículos reales
  dplyr::group_by(subfield) %>%
  dplyr::slice_head(n = 3) %>%            # 🔥 ahora sí, 3 por subfield
  dplyr::ungroup() %>%
  dplyr::select(
    subfield,
    PY = PY_final,
    TI,
    AU,
    in_degree
  )


subfield_diagnostics <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::group_by(subfield) %>%
  dplyr::summarise(
    total_nodes = n(),
    articles_with_TI = sum(!is.na(TI)),
    pct_articles = 100 * articles_with_TI / total_nodes
  ) %>%
  dplyr::arrange(desc(pct_articles))

#ARTICULOS POR SUBFILE OJO ESTE ES
get_articles_by_subfield <- function(sf) {
  
  tosr_citation_network %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    
    # 1️⃣ Subfield válido
    dplyr::filter(subfield == sf, name != "none") %>%
    
    # 2️⃣ Join con base bibliográfica
    dplyr::left_join(
      tosr_files$df,
      by = c("name" = "ID_TOS")
    ) %>%
    
    # 3️⃣ Recuperar año desde distintas fuentes
    dplyr::mutate(
      PY_join = as.character(PY.y),
      PY_name = stringr::str_extract(name, "[0-9]{4}"),
      PY_final = dplyr::coalesce(PY_join, PY_name)
    ) %>%
    
    # 4️⃣ 🔥 FILTRO CLAVE: eliminar nodos sin metadatos reales
    dplyr::filter(
      !is.na(TI),
      !is.na(AU),
      !is.na(PY_final)
    ) %>%
    
    # 5️⃣ Tabla final
    dplyr::transmute(
      subfield = sf,
      ID_TOS = name,
      PY = PY_final,
      TI,
      AU,
      SO,
      in_degree,
      out_degree
    ) %>%
    
    # 6️⃣ Relevancia
    dplyr::arrange(desc(in_degree))
}


sf1_articles <- get_articles_by_subfield(1)
View(sf1_articles)

openxlsx::write.xlsx(
  sf1_articles,
  file = "article_subfile1.xlsx",
  rowNames = FALSE
)

sf2_articles <- get_articles_by_subfield(2)
View(sf2_articles)

openxlsx::write.xlsx(
  sf2_articles,
  file = "article_subfile2.xlsx",
  rowNames = FALSE
)

sf3_articles <- get_articles_by_subfield(3)
View(sf3_articles)

openxlsx::write.xlsx(
  sf3_articles,
  file = "article_subfile3.xlsx",
  rowNames = FALSE
)


sf4_articles <- get_articles_by_subfield(4)
View(sf4_articles)

openxlsx::write.xlsx(
  sf4_articles,
  file = "article_subfile4.xlsx",
  rowNames = FALSE
)


sf5_articles <- get_articles_by_subfield(5)
View(sf5_articles)

openxlsx::write.xlsx(
  sf5_articles,
  file = "article_subfile5.xlsx",
  rowNames = FALSE
)


sf6_articles <- get_articles_by_subfield(6)
View(sf6_articles)

openxlsx::write.xlsx(
  sf6_articles,
  file = "article_subfile6.xlsx",
  rowNames = FALSE
)


sf7_articles <- get_articles_by_subfield(7)
View(sf7_articles)

openxlsx::write.xlsx(
  sf7_articles,
  file = "article_subfile7.xlsx",
  rowNames = FALSE
)


sf8_articles <- get_articles_by_subfield(8)
View(sf8_articles)

openxlsx::write.xlsx(
  sf8_articles,
  file = "article_subfile8.xlsx",
  rowNames = FALSE
)


sf9_articles <- get_articles_by_subfield(9)
View(sf9_articles)

openxlsx::write.xlsx(
  sf9_articles,
  file = "article_subfile9.xlsx",
  rowNames = FALSE
)


sf10_articles <- get_articles_by_subfield(10)
View(sf10_articles)

openxlsx::write.xlsx(
  sf10_articles,
  file = "article_subfile10.xlsx",
  rowNames = FALSE
)



sf11_articles <- get_articles_by_subfield(11)
View(sf11_articles)

openxlsx::write.xlsx(
  sf11_articles,
  file = "article_subfile11.xlsx",
  rowNames = FALSE
)


sf12_articles <- get_articles_by_subfield(12)
View(sf12_articles)

openxlsx::write.xlsx(
  sf12_articles,
  file = "article_subfile12.xlsx",
  rowNames = FALSE
)


sf13_articles <- get_articles_by_subfield(13)
View(sf13_articles)

openxlsx::write.xlsx(
  sf13_articles,
  file = "article_subfile13.xlsx",
  rowNames = FALSE
)


sf14_articles <- get_articles_by_subfield(14)
View(sf14_articles)

openxlsx::write.xlsx(
  sf14_articles,
  file = "article_subfile14.xlsx",
  rowNames = FALSE
)


sf15_articles <- get_articles_by_subfield(15)
View(sf15_articles)

openxlsx::write.xlsx(
  sf15_articles,
  file = "article_subfile15.xlsx",
  rowNames = FALSE
)


sf16_articles <- get_articles_by_subfield(16)
View(sf16_articles)

openxlsx::write.xlsx(
  sf16_articles,
  file = "article_subfile16.xlsx",
  rowNames = FALSE
)


################ INCLUYENDO KEYWORDS
colnames(tosr_files$df)


get_articles_by_subfield <- function(sf) {
  
  tosr_citation_network %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    
    dplyr::filter(subfield == sf, name != "none") %>%
    
    dplyr::left_join(
      tosr_files$df,
      by = c("name" = "ID_TOS")
    ) %>%
    
    dplyr::mutate(
      PY_join = as.character(PY.y),
      PY_name = stringr::str_extract(name, "[0-9]{4}"),
      PY_final = dplyr::coalesce(PY_join, PY_name)
    ) %>%
    
    dplyr::filter(
      !is.na(TI),
      !is.na(AU),
      !is.na(PY_final)
    ) %>%
    
    dplyr::transmute(
      subfield = sf,
      ID_TOS = name,
      PY = PY_final,
      TI,
      AU,
      SO,
      DE,   # 🔑 Author Keywords
      ID,   # 🔑 Keywords Plus
      TC,   # opcional: total citations
      in_degree,
      out_degree
    ) %>%
    
    dplyr::arrange(desc(in_degree))
}

get_top_keywords_weighted <- function(sf, top_n = 20) {
  
  get_articles_by_subfield(sf) %>%
    
    dplyr::filter(!is.na(DE)) %>%
    
    tidyr::separate_rows(DE, sep = ";") %>%
    
    dplyr::mutate(
      keyword = stringr::str_trim(stringr::str_to_lower(DE))
    ) %>%
    
    dplyr::group_by(subfield, keyword) %>%
    dplyr::summarise(
      n_articles = dplyr::n(),
      citations = sum(in_degree, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    dplyr::arrange(desc(n_articles), desc(citations)) %>%
    
    dplyr::slice_head(n = top_n)
}

kw_sf1 <- get_top_keywords_weighted("Wastewater Treatment")

kw_sf1

kw_sf1 %>%
  dplyr::slice_head(n = 10)

View(kw_sf1)

#############

sf2_articles %>%
  dplyr::summarise(
    total_nodes = n(),
    with_title = sum(!is.na(TI)),
    with_year = sum(!is.na(PY))
  )



tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(subfield == 2) %>%
  nrow()

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(subfield == 2) %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::filter(!is.na(TI)) %>%
  nrow()


sum(is.na(sample_articles_subfield$TI))
sample_articles_subfield

#################

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  group_by(subfield) %>%
  arrange(desc(in_degree)) %>%
  slice_head(n = 5) %>%
  select(subfield, name, PY, in_degree)


tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  View()

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  count(subfield, sort = TRUE)

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  select(name, subfield) %>%
  arrange(subfield)

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  filter(subfield == 10)

###

top10_subfields <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  as_tibble() %>%
  filter(!is.na(subfield), name != "none") %>%
  count(subfield, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(subfield)

figure_citation_network <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::filter(subfield %in% top10_subfields) %>%
  tidygraph::mutate(
    Subfield = factor(subfield)
  ) %>%
  ggraph::ggraph(layout = "graphopt") +
  ggraph::geom_edge_link(
    colour = "lightgray",
    alpha = 0.4
  ) +
  ggraph::geom_node_point(
    aes(
      color = Subfield,
      size  = in_degree
    )
  ) +
  ggplot2::scale_size(name = "In-degree") +
  ggraph::theme_graph() +
  ggplot2::labs(
    title = "Citation Network of the Ten Largest Subfields"
  )

figure_citation_network

####OJO!!! para graficar todos los subfiles no solo 3

tosr_files <- tosr::tosr_load("wox.txt",
                              "scopux.bib")
ToS_large <- tosr::tosSAP(graph = tosr_files$graph,
                          df = tosr_files$df,
                          nodes = tosr_files$nodes)

tosr_citation_network <- tosr_files$graph %>%
  tidygraph::as_tbl_graph() %>%
  tidygraph::activate(nodes) %>%
  dplyr::left_join(
    tosr_files$nodes,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::mutate(
    PY = stringr::str_extract(name, "[0-9]{4}"),
    in_degree  = tidygraph::centrality_degree(mode = "in"),
    out_degree = tidygraph::centrality_degree(mode = "out")
  )

tosr_graph_all <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(PY), name != "none") %>%
  dplyr::count(PY, subfield)


ggplot(tosr_graph_all,
       aes(x = PY, y = n, color = factor(subfield), group = subfield)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Subfields production through time",
    x = "Year",
    y = "Number of publications",
    color = "Subfield"
  ) +
  theme_minimal()

###para colocarlo en eje x cada 5 años

tosr_files <- tosr::tosr_load("wox.txt",
                              "scopux.bib")

ToS_large <- tosr::tosSAP(
  graph = tosr_files$graph,
  df = tosr_files$df,
  nodes = tosr_files$nodes
)

tosr_citation_network <- tosr_files$graph %>%
  tidygraph::as_tbl_graph() %>%
  tidygraph::activate(nodes) %>%
  dplyr::left_join(
    tosr_files$nodes,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::mutate(
    PY = as.numeric(stringr::str_extract(name, "[0-9]{4}")),
    in_degree  = tidygraph::centrality_degree(mode = "in"),
    out_degree = tidygraph::centrality_degree(mode = "out")
  )

tosr_graph_all <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(PY), name != "none") %>%
  dplyr::count(PY, subfield)

year_min <- min(tosr_graph_all$PY, na.rm = TRUE)
year_max <- max(tosr_graph_all$PY, na.rm = TRUE)

ggplot(tosr_graph_all,
       aes(x = PY, y = n, color = factor(subfield), group = subfield)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = sort(unique(c(
      year_min,
      seq(
        from = year_min,
        to   = year_max,
        by   = 5
      ),
      year_max
    )))
  ) +
  labs(
    title = "Subfields production through time",
    x = "Year",
    y = "Number of publications",
    color = "Subfield"
  ) +
  theme_minimal()

tosr_graph_all

##TABLA CON LOS DATOS DE LA GRAFICA
library(dplyr)
library(tidyr)
library(WriteXLS)
install.packages("writexl")   # solo una vez
library(writexl)

years_full <- data.frame(
  PY = 1923:2026
)
tosr_excel <- tosr_graph_all %>%
  # asegurar todas las combinaciones
  tidyr::complete(
    PY = 1923:2026,
    subfield,
    fill = list(n = 0)
  ) %>%
  # pasar a formato ancho
  tidyr::pivot_wider(
    names_from  = subfield,
    values_from = n,
    values_fill = 0
  ) %>%
  dplyr::arrange(PY)

tosr_excel

write_xlsx(
  tosr_excel,
  path = "subfields_publications_1923_2026.xlsx"
)

#############
sample_articles_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    subfield,
    name,
    PY,
    in_degree,
    out_degree
  )

tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  colnames()

sample_articles_subfield %>%
  dplyr::filter(is.na(TI)) %>%
  dplyr::select(name, subfield) %>%
  head(10)

sample_articles_subfield %>%
  dplyr::filter(is.na(TI)) %>%
  dplyr::select(name, subfield) %>%
  head(10)

sample_articles_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield)) %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("ID_TOS" = "ID_TOS")
  ) %>%
  dplyr::select(
    subfield,
    PY = Year,   # o PY si existe
    TI,
    AU,
    in_degree
  )

colnames(tosr_files$df)
sample_articles_subfield <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble() %>%
  dplyr::filter(!is.na(subfield), name != "none") %>%
  dplyr::group_by(subfield) %>%
  dplyr::arrange(desc(in_degree)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    PY = stringr::str_extract(name, "[0-9]{4}")
  ) %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::select(
    subfield,
    PY,
    TI,
    AU,
    SO,
    DE,
    in_degree
  )



sample_articles_subfield <- sample_articles_subfield %>%
  dplyr::left_join(
    tosr_files$df,
    by = c("name" = "ID_TOS")
  ) %>%
  dplyr::select(
    subfield,
    PY,
    TI,
    AU,
    SO,
    in_degree
  )


############alterativa para coocuerrencia entre articulos y no con referencias

library(bibliometrix)
library(tidygraph)
library(ggraph)
library(tidyverse)

# Convertir archivos a dataframe bibliometrix
M_scopus <- convert2df("scopux.bib", dbsource = "scopus", format = "bibtex")
M_wos    <- convert2df("wox.txt", dbsource = "wos", format = "plaintext")

# Unir bases
M <- mergeDbSources(
  M_scopus,
  M_wos,
  remove.duplicated = TRUE
)

# Crear red de co-ocurrencia (bibliographic coupling)
NetMatrix <- biblioNetwork(
  M,
  analysis = "coupling",
  network  = "references",
  sep = ";"
)

# Convertir a igraph
library(igraph)

g <- graph_from_adjacency_matrix(
  NetMatrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Pasar a tidygraph
g_tbl <- as_tbl_graph(g)
library(ggraph)

g_tbl %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    label  = name
  ) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(aes(size = degree), color = "steelblue") +
  scale_edge_width(range = c(0.2, 1.5)) +
  theme_graph() +
  labs(
    title = "Article Co-occurrence Network (Bibliographic Coupling)"
  )


## solo con los 119 articulos
nodes_core <- tosr_citation_network %>%
  activate(nodes) %>%
  as_tibble() %>%
  filter(!is.na(PY), name != "none")

nrow(nodes_core)


install.packages(c("tidyverse", "tosr"))
setwd("C:/Users/salva/OneDrive/Documentos/MEGA/Investigación/Proyectos/Exergy/Review/1. Arbol de la ciencia")
install.packages("bibliometrix", dependencies = TRUE)
install.packages("bibliometrix", type = "binary")
install.packages(tosr)
library(bibliometrix)
library(tidyverse)
library(plyr)
library(tosr)

M_scopus <- convert2df(
  file = "scopus.bib",
  dbsource = "scopus",
  format = "bibtex"
)

table(M_scopus$DB)
nrow(M_scopus)

M_wos <- convert2df(
  file = "WoS.bib",
  dbsource = "wos",
  format = "bibtex"
)
table(M_wos$DB)

M_wos <- convert2df(
  "wos2026.txt",
  dbsource = "wos",
  format = "plaintext"
)
nrow(M_scopus)
nrow(M_wos)


M <- rbind.fill(M_scopus, M_wos)
table(M$DB)
nrow(M)

M <- mergeDbSources(
  M_scopus,
  M_wos,
  remove.duplicated = TRUE
)

sum(is.na(M$CR))
length(M$CR)

library(tosr)

library(bibliometrix)

test_wos <- convert2df(
  "wos2026.txt",
  dbsource = "wos",
  format = "plaintext"
)

###########
wos_df <- convert2df(
  file = "wos2026.txt",
  dbsource = "wos",
  format = "plaintext"
)

scopus_df <- convert2df(
  file = "scopus.bib",
  dbsource = "scopus",
  format = "bibtex"
)

M <- mergeDbSources(
  wos_df,
  scopus_df,
  remove.duplicated = TRUE
)

M <- M %>%
  filter(!is.na(PY)) %>%      # eliminar registros sin año
  filter(!is.na(CR))          # Tree of Science requiere referencias

NetMatrix <- biblioNetwork(
  M,
  analysis = "citation",
  network  = "references",
  sep = ";"
)

NetMatrix <- biblioNetwork(
  M,
  analysis = "citation",
  network  = "references",
  sep = ";"
)

class(NetMatrix)
dim(NetMatrix)

NetMatrix <- biblioNetwork(
  M,
  analysis = "co-citation",
  network  = "references",
  sep = ";"
)

treePlot(
  NetMatrix,
  n = 30,
  type = "auto"
)

remove.packages("bibliometrix")
install.packages("bibliometrix", dependencies = TRUE)

library(tosr)

tos <- tosR(
  scopus_file = "scopus.bib",
  wos_file    = "wos2026.txt"
)

names(tos)
str(tos$TOS)
table(tos$TOS$TOS)

table(tos$TOS)
tos$cite
dim(tos$cite)
colnames(tos$cite)
tos_df <- tos$cite
tos_df$TOS <- tos$TOS

roots <- tos_df[tos_df$TOS == "Root", ]
head(roots)
nrow(roots)

tos_df <- data.frame(
  Reference = tos$cite,
  TOS = tos$TOS,
  stringsAsFactors = FALSE
)

str(tos_df)
dim(tos_df)
head(tos_df)

roots <- tos_df[tos_df$TOS == "Root", ]
nrow(roots)
head(roots)

trunk <- tos_df[tos_df$TOS == "Trunk", ]
nrow(trunk)
head(trunk)

leaves <- tos_df[tos_df$TOS == "Leaves", ]
nrow(leaves)
head(leaves)

library(ggplot2)

ggplot(tos_df, aes(x = TOS)) +
  geom_bar() +
  labs(
    title = "Tree of Science – Exergy & Wastewater",
    x = "Categoría",
    y = "Número de documentos"
  )

library(openxlsx)

write.xlsx(roots,  "ToS_Roots.xlsx",  overwrite = TRUE)
write.xlsx(trunk,  "ToS_Trunk.xlsx",  overwrite = TRUE)
write.xlsx(leaves, "ToS_Leaves.xlsx", overwrite = TRUE)


library(bibliometrix)

NetMatrix <- biblioNetwork(
  M,
  analysis = "co-citation",
  network  = "references",
  sep = ";"
)

networkPlot(
  NetMatrix,
  n = 30,
  type = "auto",
  size = TRUE,
  labelsize = 0.7
)

# Normalizar títulos
M$TI_clean <- tolower(trimws(M$TI))

# Eliminar duplicados por título
M_clean <- M[!duplicated(M$TI_clean), ]

# Eliminar columna auxiliar
M_clean$TI_clean <- NULL

nrow(M)
nrow(M_clean)
table(M_clean$DB)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(
  M_clean,
  file = "Base_exergy_wastewater_clean.xlsx",
  overwrite = TRUE
)

getwd()


colnames(M_clean)

######### ARBOL DE LA CIENCIA + WEYWORDS
tosr_files$df$ID_TOS
ToS_short$ID
colnames(ToS_short)

colnames(tosr_files$nodes)
head(tosr_files$nodes)
ToS_keywords <- tosr_files$nodes %>%
  
  dplyr::left_join(
    tosr_files$df %>% dplyr::select(ID_TOS, DE, ID),
    by = "ID_TOS"
  ) %>%
  
  dplyr::mutate(
    keywords = dplyr::coalesce(DE, ID)
  )
ToS_keywords %>%
  dplyr::select(ID_TOS, role, keywords) %>%
  dplyr::slice_head(n = 10)

colnames(tosr_files$nodes)
head(tosr_files$nodes, 5)

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Unir nodos del ToS con keywords
ToS_keywords <- tosr_files$nodes %>%
  
  left_join(
    tosr_files$df %>%
      select(ID_TOS, DE, ID),   # Author Keywords y Keywords Plus
    by = "ID_TOS"
  ) %>%
  
  mutate(
    keywords = coalesce(DE, ID)  # Combinar DE + ID
  )

# Limpiar y separar keywords
ToS_keywords_long <- ToS_keywords %>%
  filter(!is.na(keywords)) %>%
  separate_rows(keywords, sep = ";") %>%
  mutate(
    keyword = str_trim(str_to_lower(keywords))
  )

# Ver los primeros artículos con keywords
head(ToS_keywords_long %>% select(ID_TOS, keyword), 10)

# Exportar a Excel
write.xlsx(ToS_keywords_long,
           "ToS_keywords.xlsx",
           overwrite = TRUE)

library(tidygraph)

tosr_citation_network <- tosr_files$graph %>%
  tidygraph::as_tbl_graph() %>%
  tidygraph::activate(nodes) %>%
  left_join(tosr_files$nodes, by = c("name" = "ID_TOS")) %>%
  mutate(
    in_degree = tidygraph::centrality_degree(mode = "in")
  ) %>%
  mutate(
    role = case_when(
      in_degree == max(in_degree) ~ "root",
      in_degree > median(in_degree) ~ "trunk",
      TRUE ~ "leaves"
    )
  )

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Convertimos los nodos a tibble
nodes_tbl <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::as_tibble()

# Calculamos in_degree y rol
nodes_tbl <- nodes_tbl %>%
  mutate(
    in_degree = tidygraph::centrality_degree(tosr_citation_network, mode = "in"),
    role = case_when(
      in_degree == max(in_degree) ~ "root",
      in_degree > median(in_degree) ~ "trunk",
      TRUE ~ "leaves"
    )
  )

# Unimos keywords
ToS_keywords <- nodes_tbl %>%
  left_join(
    tosr_files$df %>% select(ID_TOS, DE, ID),
    by = c("name" = "ID_TOS")
  ) %>%
  mutate(keywords = coalesce(DE, ID)) %>%
  filter(!is.na(keywords)) %>%
  separate_rows(keywords, sep = ";") %>%
  mutate(keyword = str_trim(str_to_lower(keywords)))

# Ver los primeros
head(ToS_keywords %>% select(name, role, keyword), 10)

# Exportar a Excel
write.xlsx(ToS_keywords,
           "ToS_keywords_with_roles.xlsx",
           overwrite = TRUE)

library(tidygraph)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# 1️⃣ Calcular in_degree y rol directamente sobre el graph
nodes_tbl <- tosr_citation_network %>%
  tidygraph::activate(nodes) %>%
  tidygraph::mutate(
    in_degree = centrality_degree(mode = "in"),
    role = case_when(
      in_degree == max(in_degree) ~ "root",
      in_degree > median(in_degree) ~ "trunk",
      TRUE ~ "leaves"
    )
  ) %>%
  tidygraph::as_tibble()  # Convertimos a tibble después

# 2️⃣ Unir keywords
ToS_keywords <- nodes_tbl %>%
  left_join(
    tosr_files$df %>% select(ID_TOS, DE, ID),
    by = c("name" = "ID_TOS")
  ) %>%
  mutate(keywords = coalesce(DE, ID)) %>%
  filter(!is.na(keywords)) %>%
  separate_rows(keywords, sep = ";") %>%
  mutate(keyword = str_trim(str_to_lower(keywords)))

# 3️⃣ Ver resultado
head(ToS_keywords %>% select(name, role, keyword), 10)

# 4️⃣ Exportar a Excel
write.xlsx(ToS_keywords,
           "ToS_keywords_with_roles.xlsx",
           overwrite = TRUE)


library(dplyr)
library(tidyr)
library(stringr)
library(tidygraph)
library(openxlsx)

# 1️⃣ Calcular in_degree y rol dentro del ToS
nodes_tbl <- tosr_citation_network %>%
  activate(nodes) %>%
  mutate(
    in_degree = centrality_degree(mode = "in"),
    role = case_when(
      in_degree == max(in_degree) ~ "root",
      in_degree > median(in_degree) ~ "trunk",
      TRUE ~ "leaves"
    )
  ) %>%
  as_tibble()

# 2️⃣ Unir keywords solo para los artículos del ToS
ToS_keywords <- nodes_tbl %>%
  left_join(
    tosr_files$df %>% select(ID_TOS, DE, ID),
    by = c("name" = "ID_TOS")
  ) %>%
  mutate(keywords = coalesce(DE, ID)) %>%
  filter(!is.na(keywords)) %>%
  separate_rows(keywords, sep = ";") %>%
  mutate(keyword = str_trim(str_to_lower(keywords)))

# 3️⃣ Ver primeros 10
head(ToS_keywords %>% select(name, role, keyword, in_degree), 10)

# 4️⃣ Exportar a Excel
write.xlsx(ToS_keywords,
           "ToS_keywords_with_roles_justtos.xlsx",
           overwrite = TRUE)



