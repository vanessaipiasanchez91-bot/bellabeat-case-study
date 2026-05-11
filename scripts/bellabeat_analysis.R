# =========================================
# 📊 Bellabeat Case Study
# Author: Vanessa Ipia Sanchez
# Date: 2026
# =========================================

# 📦 Librerías
library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)

# 📂 Cargar datos
activity <- read_csv("data/dailyActivity_merged.csv")

# 👀 Exploración inicial
head(activity)
glimpse(activity)
summary(activity)

# 🧹 Limpieza
activity <- clean_names(activity)

# 🔍 Verificar datos
colSums(is.na(activity))
nrow(activity)
nrow(distinct(activity))

# 📅 Transformaciones
activity <- activity %>%
  mutate(activity_date = mdy(activity_date))

activity <- activity %>%
  mutate(day_of_week = weekdays(activity_date))

# 🔢 Ordenar días de la semana
activity$day_of_week <- factor(activity$day_of_week,
levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
)

# 📊 Métrica básica
mean(activity$total_steps)

# 🪑 Crear nivel de actividad
activity <- activity %>%
  mutate(activity_level = case_when(
    total_steps < 5000 ~ "Sedentario",
    total_steps < 10000 ~ "Moderado",
    TRUE ~ "Activo"
  ))

# 📈 Visualización

# 📊 Gráfico de barras: Promedio de pasos por día
g1 <- activity %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(total_steps)) %>%
  ggplot(aes(x = day_of_week, y = avg_steps)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Promedio de pasos por día",
    x = "Día de la semana",
    y = "Pasos promedio"
  ) +
  theme_minimal()

# 📈 Gráfico de línea: Tendencia semanal de pasos
g2 <- activity %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(total_steps)) %>%
  ggplot(aes(x = day_of_week, y = avg_steps, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Tendencia de pasos durante la semana",
    x = "Día de la semana",
    y = "Pasos promedio"
  ) +
  theme_minimal()

# 🔵 Gráfico de dispersión: Relación pasos vs calorías
g3<- ggplot(activity, aes(x = total_steps, y = calories, color = activity_level)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Relación entre pasos y calorías",
    x = "Pasos",
    y = "Calorías",
    color = "Nivel de actividad"
  ) +
  theme_minimal()

# 📊 Gráfico de barras: Porcentaje de nivel de actividad
g4 <- activity %>%
  count(activity_level) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = activity_level, y = percent, fill = activity_level)) +
  geom_col() +
  geom_text(aes(label = round(percent, 1)), vjust = -0.5) +
  labs(
    title = "Distribución porcentual de niveles de actividad",
    x = "Nivel de actividad",
    y = "Porcentaje"
  ) +
  theme_minimal()


# 📊 Visualización combinada
     (g1 | g2) / (g3 | g4)


# 💾 Guardar gráfica
ggsave("output/pasos_por_dia.png", width = 8, height = 5)
ggsave("output/tendencia_pasos.png", width = 8, height = 5)
ggsave("output/pasos_vs_calorias.png", width = 8, height = 5)
