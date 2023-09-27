library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)
library(stringr)
library(tibble)
library(ordinal)
# library(scales)
# library(tibble)
# library(forcats)
# library(MASS)
# library(lme4)
# library(factoextra)
# library(kableExtra)
# library(ltm)

output_dir <- "output/"
if (!dir.exists(output_dir)) {
    dir.create(output_dir)
}

save_pdf <- FALSE

# To ensure reproducibility
set.seed(12345)

answers <- read.csv("survey_results.csv")

# Graphical parameters

# Generic theme for all plots
plot_theme <- theme_light() +
    theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12, colour = "black")
    )

# Palettes
fill_palette <- c("#404040", "#ACACAC")
pts_palette <- c("#303030", "#808080")

################ FIGURE 1 ################

# Plot of % students in each University per year of study
if (save_pdf) {
    pdf(paste0(output_dir, "Figure 1.pdf"), width = 15, height = 5)
}

f1_a <- table(Country = answers$Country, Year = answers$Year) %>%
    as.data.frame() %>%
    group_by(Country) %>%
    mutate(Percent = Freq / sum(Freq) * 100) %>%
    ggplot(aes(x = Year, y = Percent, fill = Country)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = fill_palette) +
    ylab("Percentage of students") +
    xlab("Year of study") +
    scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
    plot_theme

# Plot of % students in each University per level of experience
f1_b <- table(Country = answers$Country, Year = answers$Experience) %>%
    as.data.frame() %>%
    group_by(Country) %>%
    mutate(Percent = Freq / sum(Freq) * 100) %>%
    ggplot(aes(x = Year, y = Percent, fill = Country)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = fill_palette) +
    ylab("Percentage of students") +
    xlab("Experience") +
    scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
    plot_theme

ggarrange(f1_a, f1_b,
    ncol = 2,
    labels = "AUTO",
    label.x = -.02,
    font.label = list(size = 30),
    common.legend = TRUE
)

if (save_pdf) {
    dev.off()
}

################ FIGURE 2 ################

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 2.pdf"), width = 15, height = 8)
}

# Compassion by experience
f2_a <- answers %>%
    dplyr::filter(!is.na(Experience)) %>%
    ggplot(aes(x = factor(Experience), y = Compassion), outlier.shape = NA) +
    geom_boxplot(aes(fill = Country),
        alpha = 0.3, outlier.size = 0,
        position = position_dodge2(preserve = "single")
    ) +
    scale_fill_manual(values = fill_palette) +
    geom_point(aes(col = Country),
        position = position_jitterdodge(jitter.width = 0.1)
    ) +
    scale_color_manual(values = pts_palette) +
    xlab("Experience") +
    ylab("Compassion score") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0), minor_breaks = 0) +
    plot_theme

# Compassion by year of study
f2_b <- ggplot(answers, aes(x = factor(Year), y = Compassion)) +
    geom_boxplot(aes(fill = Country), alpha = 0.3, outlier.shape = NA) +
    scale_fill_manual(values = fill_palette) +
    geom_point(aes(col = Country),
        position = position_jitterdodge(jitter.width = 0.1)
    ) +
    scale_color_manual(values = pts_palette) +
    xlab("Year of study") +
    ylab("Compassion score") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0), minor_breaks = 0) +
    plot_theme

# Compassion by gender (only showing M/F)
f2_c <- answers %>%
    dplyr::filter(Gender %in% c("Male", "Female")) %>%
    ggplot(aes(x = factor(Gender), y = Compassion)) +
    geom_boxplot(aes(fill = Country), alpha = 0.3, outlier.shape = NA) +
    scale_fill_manual(values = fill_palette) +
    geom_point(aes(col = Country),
        position = position_jitterdodge(jitter.width = 0.1)
    ) +
    scale_color_manual(values = pts_palette) +
    xlab("") +
    ylab("Compassion score") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0), minor_breaks = 0) +
    plot_theme

# Compassion by country
f2_d <- ggplot(answers, aes(x = Country, y = Compassion)) +
    geom_boxplot(aes(fill = Country), alpha = 0.3, outlier.shape = NA) +
    scale_fill_manual(values = fill_palette) +
    geom_point(aes(col = Country),
        position = position_jitterdodge(jitter.width = 0.1)
    ) +
    scale_color_manual(values = pts_palette) +
    xlab("Experience") +
    ylab("Compassion score") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0), minor_breaks = 0) +
    plot_theme

ggarrange(f2_a, f2_b, f2_c, f2_d,
    ncol = 2, nrow = 2,
    labels = "AUTO",
    label.x = -.02,
    label.y = 1.05,
    font.label = list(size = 30),
    common.legend = TRUE
)

if (save_pdf) {
    dev.off()
}

################ FIGURE 3 ################

answers_suff <- answers %>%
    dplyr::select(c(
        ID, University, Country, Gender,
        Year, Experience, starts_with("Q_suff")
    )) %>%
    pivot_longer(
        cols = starts_with("Q_suff"),
        names_to = "Question", values_to = "Score"
    ) %>%
    mutate(Question = sub("Q_suff_", "", Question)) %>%
    mutate(Disease = case_when(
        Question == 1 ~ "Diabetes",
        Question == 2 ~ "FOP",
        Question == 3 ~ "Psoriasis"
    )) %>%
    mutate(NumAffected = case_when(
        Question == 1 ~ 3,
        Question == 2 ~ 1,
        Question == 3 ~ 2
    )) %>%
    mutate(Severity = case_when(
        Question == 1 ~ 2,
        Question == 2 ~ 3,
        Question == 3 ~ 1
    )) %>%
    mutate(ID = factor(ID))

# Percentage of students who justify the animal to suffer at
# different levels of severity by disease and country

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 3.pdf"), width = 15, height = 10)
}

f3_a <- answers_suff %>%
    group_by(Country, Disease, Score) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(Country, Disease) %>%
    mutate(perc = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(
        x = factor(Disease, levels = c("Psoriasis", "Diabetes", "FOP")),
        y = perc, fill = factor(Score, levels = 3:0)
    )) +
    scale_fill_manual(
        values = gray.colors(4), name = "Suffering",
        labels = c("None", "Mild", "Moderate", "Severe")
    ) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~Country, ncol = 2) +
    xlab("Disease\n(ordered by severity)") +
    ylab("Percentage") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Average severity score by year of study and country
f3_b <- answers_suff %>%
    group_by(Year, Country) %>%
    summarise(
        mean_score = mean(as.integer(Score)),
        sem_score = sd(as.integer(Score)) / sqrt(n())
    ) %>%
    ungroup() %>%
    ggplot(aes(x = Year, y = mean_score)) +
    geom_line(aes(col = Country)) +
    geom_segment(aes(
        x = Year, xend = Year, y = mean_score - sem_score,
        yend = mean_score + sem_score, col = Country
    )) +
    scale_color_manual(values = fill_palette) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    ylab("Mean score") +
    xlab("Year of study") +
    plot_theme

# Average severity score by gender and country
f3_c <- answers_suff %>%
    dplyr::filter(Gender %in% c("Male", "Female")) %>%
    group_by(Gender, Country) %>%
    summarise(
        mean_score = mean(as.integer(Score)),
        sem_score = sd(as.integer(Score)) / sqrt(n())
    ) %>%
    ungroup() %>%
    ggplot(aes(x = Gender, y = mean_score)) +
    geom_point(aes(col = Country), cex = 2) +
    geom_segment(aes(
        x = Gender, xend = Gender, y = mean_score - sem_score,
        yend = mean_score + sem_score, col = Country
    )) +
    scale_color_manual(values = fill_palette) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    ylab("Mean score") +
    xlab("Experience") +
    plot_theme

# Average severity score by experience and country
f3_d <- answers_suff %>%
    group_by(Experience, Country) %>%
    summarise(
        mean_score = mean(as.integer(Score)),
        sem_score = sd(as.integer(Score)) / sqrt(n())
    ) %>%
    ungroup() %>%
    ggplot(aes(x = Experience, y = mean_score)) +
    geom_line(aes(col = Country)) +
    geom_segment(aes(
        x = Experience, xend = Experience, y = mean_score - sem_score,
        yend = mean_score + sem_score, col = Country
    )) +
    scale_color_manual(values = fill_palette) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    ylab("Mean score") +
    xlab("Experience") +
    plot_theme

ggarrange(f3_a, f3_b, f3_c, f3_d,
    ncol = 2, nrow = 2,
    labels = "AUTO",
    label.x = -.02,
    font.label = list(size = 30)
)

if (save_pdf) {
    dev.off()
}

################ FIGURE 4 ################

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 4.pdf"), width = 12, height = 12)
}

answers_suff %>%
    mutate(Compassion = answers$Compassion[ID]) %>%
    ggplot(aes(x = Score, y = Compassion)) +
    geom_jitter(width = 0.15) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Disease, ncol = 2) +
    ylab("Compassion score") +
    xlab("Score on suffering questions") +
    scale_y_continuous(
        limits = c(0, 60), expand = c(0, 0),
        minor_breaks = 0
    ) +
    plot_theme

if (save_pdf) {
    dev.off()
}

# Correlation test between compassion and suffering score by Disease
answers_suff %>%
    group_by(Disease) %>%
    left_join(answers %>%
        dplyr::select(ID, Compassion), by = "ID") %>%
    summarise(
        r = cor.test(Score, Compassion, method = "pearson")$estimate,
        p = cor.test(Score, Compassion, method = "pearson")$p.value
    ) %>%
    mutate(
        padj = p.adjust(p, method = "holm"),
    ) %>%
    # format p-values
    mutate(
        p = formatC(p, format = "e", digits = 2),
        padj = formatC(padj, format = "e", digits = 2)
    )


################ FIGURES 5 AND 6 ################

answers_cons <- answers %>%
    dplyr::select(c(
        ID, University, Country, Gender, Year,
        Experience, starts_with("Q_cons")
    )) %>%
    pivot_longer(
        cols = starts_with("Q_cons"),
        names_to = "Question", values_to = "Score"
    ) %>%
    mutate(Question = as.integer(str_replace(Question, "Q_cons_", "")) + 16) %>%
    mutate(Topic = case_when(
        Question %in% c(17, 19, 21, 23) ~ "Consciousness",
        Question %in% c(18, 20, 22, 24) ~ "Emotion"
    )) %>%
    mutate(Species = case_when(
        Question %in% c(17, 18) ~ "Human (self)",
        Question %in% c(19, 20) ~ "Human (other)",
        Question %in% c(21, 22) ~ "Rat",
        Question %in% c(23, 24) ~ "Fruit fly"
    )) %>%
    # We make this a factor rather than an ordered factor as
    # it makes the GLM models more esplicable
    mutate(Species = factor(Species,
        levels = c("Human (self)", "Human (other)", "Rat", "Fruit fly")
    ))

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 5.pdf"), width = 12, height = 12)
}

# Scores for consciousness by species and country
answers_cons %>%
    dplyr::filter(Topic == "Consciousness") %>%
    group_by(Country, Species, Score) %>%
    summarise(n = n()) %>%
    mutate(perc = n / sum(n) * 100) %>%
    ggplot(aes(x = factor(Score), y = perc)) +
    geom_col(aes(fill = Country),
        position = position_dodge2(preserve = "single")
    ) +
    scale_fill_manual(values = fill_palette) +
    facet_wrap(~Species, ncol = 2) +
    ylab("Percentage") +
    xlab("Score") +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    plot_theme +
    theme(legend.position = "bottom")

if (save_pdf) {
    dev.off()
}

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 6.pdf"), width = 12, height = 12)
}

# Scores for emotion by species and country
answers_cons %>%
    dplyr::filter(Topic == "Emotion") %>%
    group_by(Country, Species, Score) %>%
    summarise(n = n()) %>%
    mutate(perc = n / sum(n) * 100) %>%
    ggplot(aes(x = factor(Score), y = perc)) +
    geom_col(aes(fill = Country),
        position = position_dodge2(preserve = "single")
    ) +
    scale_fill_manual(values = fill_palette) +
    facet_wrap(~Species, ncol = 2) +
    ylab("Percentage") +
    xlab("Score") +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    plot_theme +
    theme(legend.position = "bottom")

if (save_pdf) {
    dev.off()
}

################ FIGURE 7 ################

if (save_pdf) {
    pdf(paste0(output_dir, "Figure 7.pdf"), width = 12, height = 6)
}

# Correlation between compassion scores and scores for consciousness
f7a <- answers_cons %>%
    dplyr::filter(Topic == "Consciousness") %>%
    # Join with compassion scores on answers
    left_join(answers %>%
        dplyr::select(ID, Compassion), by = "ID") %>%
    ggplot(aes(x = Score, y = Compassion)) +
    geom_jitter(width = 0.15) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Species, ncol = 2) +
    ylab("Compassion score") +
    xlab("Score on consciousness questions") +
    scale_y_continuous(
        limits = c(0, 60), expand = c(0, 0),
        minor_breaks = 0
    ) +
    plot_theme

# Correlation between compassion scores and scores for emotion
f7b <- answers_cons %>%
    dplyr::filter(Topic == "Emotion") %>%
    # Join with compassion scores on answers
    left_join(answers %>%
        dplyr::select(ID, Compassion), by = "ID") %>%
    ggplot(aes(x = Score, y = Compassion)) +
    geom_jitter(width = 0.15) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Species, ncol = 2) +
    ylab("Compassion score") +
    xlab("Score on emotion questions") +
    scale_y_continuous(
        limits = c(0, 60), expand = c(0, 0),
        minor_breaks = 0
    ) +
    plot_theme

ggarrange(f7a, f7b,
    ncol = 2, nrow = 1,
    labels = "AUTO",
    label.x = -.02,
    font.label = list(size = 30)
)

if (save_pdf) {
    dev.off()
}

################ STATISTICS ################

get_polr_pvalues <- function(model) {
    #' Gets p-values for ordinal logistic regression
    #' @param model An ordinal logistic regression model created with polr
    #' @return A vector of p-values
    ctable <- coef(summary(model))
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    return(p)
}

get_or <- function(model) {
    # The variables of our model
    mod_vars <- rownames(summary(model)$coefficients)

    # We need to use confint.default because confint does not work with
    # ordinal::clmm models
    ci <- confint.default(model)
    ci <- ci[rownames(ci) %in% mod_vars]
    mod_coeff <- coef(model)
    mod_coeff <- mod_coeff[names(mod_coeff) %in% mod_vars]
    # This is needed when we only have one predictor
    ci_mtx <- matrix(ci, ncol = 2)
    colnames(ci_mtx) <- c("CI 2.5%", "97.5%")

    return(exp(cbind(OR = mod_coeff, CI = ci_mtx)))
}

# Compassion scores GLM
model_comp <- glm(
    cbind(Compassion, 60 - Compassion) ~ University +
        Gender + Year + Experience,
    data = answers, family = quasibinomial(link = "logit")
)

# Table 1
get_or(model_comp) %>%
    as.data.frame() %>%
    rownames_to_column("Term") %>%
    cbind("p value" = coef(summary(model_comp))[, "Pr(>|t|)"]) %>%
    dplyr::filter(!Term %in% c(
        "(Intercept)", "GenderOther",
        "GenderPrefer not to say"
    )) %>%
    dplyr::select(-Term) %>%
    format(digits = 3)

# Suffering scores ordinal model
model_suff <- clmm2(
    ordered(Score) ~ NumAffected + Severity +
        University + Year + Experience + Gender,
    random = ID, data = answers_suff, Hess = TRUE
)

get_or(model_suff) %>%
    as.data.frame() %>%
    rownames_to_column("Term") %>%
    cbind("p value" = coef(summary(model_suff))[, "Pr(>|z|)"]) %>%
    dplyr::filter(!Term %in% c(
        "(Intercept)", "GenderOther",
        "GenderPrefer not to say"
    )) %>%
    dplyr::select(-Term) %>%
    # Remove ordinal coefficients (first 3 rows)
    dplyr::slice(-c(1:3)) %>%
    format(digits = 3)

# Consciousness scores ordinal model
model_cons <- clmm2(
    ordered(Score) ~ Species +
        University + Year + Experience + Gender,
    random = factor(ID), data = answers_cons %>%
        filter(Topic == "Consciousness"), Hess = TRUE
)

get_or(model_cons) %>%
    as.data.frame() %>%
    rownames_to_column("Term") %>%
    cbind("p value" = coef(summary(model_cons))[, "Pr(>|z|)"]) %>%
    dplyr::filter(!Term %in% c(
        "(Intercept)", "GenderOther",
        "GenderPrefer not to say"
    )) %>%
    dplyr::select(-Term) %>%
    # Remove ordinal coefficients (first 3 rows)
    dplyr::slice(-c(1:6)) %>%
    format(digits = 3)

# Emotion scores ordinal model
model_emo <- clmm2(
    ordered(Score) ~ Species +
        University + Year + Experience + Gender,
    random = factor(ID), data = answers_cons %>%
        filter(Topic == "Emotion"), Hess = TRUE
)

get_or(model_emo) %>%
    as.data.frame() %>%
    rownames_to_column("Term") %>%
    cbind("p value" = coef(summary(model_emo))[, "Pr(>|z|)"]) %>%
    dplyr::filter(!Term %in% c(
        "(Intercept)", "GenderOther",
        "GenderPrefer not to say"
    )) %>%
    dplyr::select(-Term) %>%
    # Remove ordinal coefficients (first 3 rows)
    dplyr::slice(-c(1:6)) %>%
    format(digits = 3)

################ SUPPLEMENTARY FIGURES ################

# Figure S1 - Correlation between year of study and experience level
if (save_pdf) {
    pdf(paste0(output_dir, "Supplementary Figure 1.pdf"),
        width = 8, height = 8
    )
}

answers %>%
    ggplot(aes(x = Year, y = Experience)) +
    geom_jitter(width = 0.07, height = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    ylab("Experience") +
    xlab("Year of study") +
    scale_y_continuous(
        limits = c(0, 5), expand = c(0, 0),
        minor_breaks = 0
    ) +
    plot_theme

if (save_pdf) {
    dev.off()
}

cor.test(answers$Year, answers$Experience, method = "pearson")

# Figure S2 - PCA
answers %>%
    dplyr::select(starts_with("Q_comp_")) %>%
    as.matrix() %>%
    prcomp(center = TRUE, scale = TRUE) -> pca_q_comp

pca_df <- data.frame(
    PC1 = pca_q_comp$rotation[, "PC1"],
    PC2 = pca_q_comp$rotation[, "PC2"]
) %>%
    rownames_to_column("Question") %>%
    mutate(Question = as.integer(gsub("Q_comp_", "", Question))) %>%
    mutate(Empathetic = ifelse(Question %in% c(2, 5, 6, 9, 10, 12), 
        "Anti-empathetic", "Empathetic")) %>%
    mutate(Empathetic = factor(Empathetic, 
        levels = c("Empathetic", "Anti-empathetic")))

if (save_pdf) {
    pdf(paste0(output_dir, "Supplementary Figure 2.pdf"),
        width = 8, height = 8
    )
}

ggplot(pca_df, aes(x = PC1, y = PC2)) +
    geom_point(aes(col = Empathetic), size = 3) +
    scale_color_manual(values = c("lightgray", "black")) +
    geom_text(aes(label = paste0("Q", Question)),
        size = 5,
        nudge_x = 0.02, nudge_y = -0.01
    ) +
    xlab("PC1") +
    ylab("PC2") +
    plot_theme +
    theme(legend.position = "bottom",
        legend.title = element_blank())

if (save_pdf) {
    dev.off()
}

# Figure S3 - Attribution of (A) consciousness and (B) emotions to self,
# other humans, rats and fruit flies by Year of Study.

fs3_a <- answers_cons %>%
    dplyr::filter(Topic == "Consciousness") %>%
    # Calculate the n per year
    group_by(Species, Year, Score) %>%
    summarise(n_sp_year = n()) %>%
    mutate(perc = n_sp_year / sum(n_sp_year) * 100) %>%
    ungroup() %>%
    ggplot(aes(x = factor(Score), y = perc)) +
    geom_col() +
    facet_wrap(paste("Year ", Year) ~ Species, ncol = 4) +
    ylab("Percentage") +
    xlab("Score") +
    plot_theme +
    theme(legend.position = "bottom")

fs3_b <- answers_cons %>%
    dplyr::filter(Topic == "Emotion") %>%
    # Calculate the n per year
    group_by(Species, Year, Score) %>%
    summarise(n_sp_year = n()) %>%
    mutate(perc = n_sp_year / sum(n_sp_year) * 100) %>%
    ungroup() %>%
    ggplot(aes(x = factor(Score), y = perc)) +
    geom_col() +
    facet_wrap(paste("Year ", Year) ~ Species, ncol = 4) +
    ylab("Percentage") +
    xlab("Score") +
    plot_theme +
    theme(legend.position = "bottom")

if (save_pdf) {
    pdf(paste0(output_dir, "Supplementary Figure 3.pdf"),
        width = 8, height = 16
    )
}

ggarrange(fs3_a, fs3_b,
    ncol = 1, nrow = 2,
    labels = "AUTO",
    label.x = -.02,
    font.label = list(size = 30)
)

if (save_pdf) {
    dev.off()
}

# Figure S4 - Correlations between consciousness/emotion
# and compassion scores

if (save_pdf) {
    pdf(paste0(output_dir, "Supplementary Figure 4.pdf"),
        width = 12, height = 8
    )
}

answers_cons %>%
    left_join(answers %>%
        dplyr::select(ID, Compassion), by = "ID") %>%
    ggplot(aes(x = Score, y = Compassion, col = Topic)) +
    geom_point(position = position_jitterdodge(
        jitter.width = 0.1,
        dodge.width = 0.8
    )) +
    scale_color_manual(values = pts_palette) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Species, ncol = 2) +
    ylab("Compassion score") +
    xlab("Score on consciousness/emotion questions") +
    scale_y_continuous(
        limits = c(0, 60), expand = c(0, 0),
        minor_breaks = 0
    ) +
    plot_theme

if (save_pdf) {
    dev.off()
}

# Correlation test
answers_cons %>%
    left_join(answers %>%
        dplyr::select(ID, Compassion), by = "ID") %>%
    group_by(Species, Topic) %>%
    summarise(
        r = cor.test(Score, Compassion, method = "pearson")$estimate,
        p = cor.test(Score, Compassion, method = "pearson")$p.value
    ) %>%
    mutate(
        padj = p.adjust(p, method = "holm"),
    ) %>%
    mutate(
        p = format(p, digits = 3),
        padj = format(padj, digits = 3)
    )