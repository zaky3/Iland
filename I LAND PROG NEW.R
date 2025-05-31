
library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(leaflet)
library(DT)

user <- data.frame(
  username = c("admin", "user1"),
  password = c("admin123", "password1"),
  stringsAsFactors = FALSE
)

# Data harga lengkap dengan rentang harga dan koordinat (latitude, longitude) untuk peta (koordinat disederhanakan)
harga_lengkap <- data.frame(
  Provinsi_Kawasan = c(
    "Banten", "Banten", "Banten",
    "Jawa Barat", "Jawa Barat",
    "DKI Jakarta", "DKI Jakarta", "DKI Jakarta", "DKI Jakarta", "DKI Jakarta",
    "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta",
    "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta", "DI Yogyakarta",
    "DI Yogyakarta", "DI Yogyakarta",
    "Jawa Tengah",
    "Jawa Timur",
    "Bali",
    "Sumatera Utara",
    "Kalimantan Timur",
    "Sulawesi Selatan",
    "Bogor (Jawa Barat)", "Bogor (Jawa Barat)", "Bogor (Jawa Barat)", "Bogor (Jawa Barat)", "Bogor (Jawa Barat)",
    "Depok (Jawa Barat)", "Depok (Jawa Barat)", "Depok (Jawa Barat)", "Depok (Jawa Barat)",
    "Tangerang (Banten)", "Tangerang (Banten)",
    "Bekasi (Jawa Barat)", "Bekasi (Jawa Barat)", "Bekasi (Jawa Barat)", "Bekasi (Jawa Barat)"
  ),
  Wilayah_Lokasi = c(
    "Pandeglang, Lebak", "Pinggiran Serang", "Rata-rata Kab. Lebak",
    "Banjaran (Kab. Bandung)", "Rata-rata Provinsi Jawa Barat",
    "Jakarta Selatan", "Jakarta Timur", "Jakarta Barat", "Jakarta Utara", "Jakarta Pusat",
    "Sleman, Bantul, Kulon Progo (umum)", "Malioboro & Sekitarnya", "Sleman", "Bantul", "Kotagede",
    "Condongcatur", "Jalan Kaliurang", "Prawirotaman", "Timoho", "Demangan",
    "Giwangan", "Babarsari",
    "Sekitar Semarang, Solo",
    "Sekitar Malang, Pasuruan, Batu",
    "Gianyar, Tabanan, Buleleng",
    "Deli Serdang, Langkat",
    "Sekitar Balikpapan, Samarinda",
    "Sekitar Makassar",
    "Bogor Selatan", "Bogor Utara", "Bogor Tengah", "Bogor Barat", "Bogor Timur",
    "Depok Timur", "Depok Selatan", "Depok Barat", "Depok Utara",
    "Tangerang Selatan", "Tangerang Kota",
    "Bekasi Selatan", "Bekasi Timur", "Bekasi Barat", "Bekasi Utara"
  ),
  Harga_Min = c(
    300000, 1000000, 720000,
    175000, 3500000,
    40000000, 30000000, 25000000, 20000000, 35000000,
    500000, 25000000, 2000000, 1000000, 10000000,
    15000000, 15000000, 10000000, 15000000, 10000000,
    5000000, 15000000,
    350000, 400000, 700000,
    200000, 300000,
    350000,
    6000000, 4000000, 5000000, 4000000, 5000000,
    4000000, 5000000, 3000000, 4000000,
    4000000, 5000000,
    3000000, 2500000, 3000000, 2500000
  ),
  Harga_Max = c(
    700000, 1000000, 720000,
    10000000, 3500000,
    60000000, 45000000, 40000000, 35000000, 50000000,
    2000000, 50000000, 15000000, 10000000, 25000000,
    30000000, 30000000, 20000000, 30000000, 20000000,
    15000000, 30000000,
    1200000, 1500000, 3000000,
    900000, 1500000,
    1000000,
    10000000, 8000000, 9000000, 7000000, 8000000,
    8000000, 10000000, 7000000, 9000000,
    8000000, 9000000,
    6000000, 5000000, 6000000, 5000000
  ),
  lat = c(
    -6.3, -6.1, -6.3,
    -7.1, -6.9,
    -6.3, -6.2, -6.2, -6.1, -6.2,
    -7.7, -7.8, -7.7, -7.9, -7.8,
    -7.8, -7.8, -7.8, -7.8, -7.8,
    -7.9, -7.9,
    -7.0, -7.9, -8.5,
    3.6, 1.2,
    -5.1,
    -6.6, -6.5, -6.6, -6.6, -6.6,
    -6.4, -6.4, -6.4, -6.4,
    -6.3, -6.2,
    -6.2, -6.2, -6.2, -6.2
  ),
  lng = c(
    105.9, 106.1, 105.9,
    107.5, 107.6,
    106.8, 106.9, 106.7, 106.8, 106.8,
    110.4, 110.4, 110.4, 110.3, 110.4,
    110.4, 110.4, 110.4, 110.4, 110.4,
    110.4, 110.4,
    110.4, 112.6, 115.2,
    98.7, 116.8,
    119.4,
    106.8, 106.8, 106.8, 106.8, 106.8,
    106.8, 106.8, 106.8, 106.8,
    106.7, 106.7,
    106.9, 106.9, 106.9, 106.9
  ),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
 	 html, body {
      height: 100%;
      margin: 0;
      font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
      background-color: #f8f9fa;
  	}
  	.center-wrapper {
      height: 100vh;
      display: flex;
      justify-content: center;
      align-items: center;
      background: linear-gradient(135deg, #ede7f6, #fce4ec);
      padding: 20px;
  	}
  	.main-box {
      background-color: #ffffff;
      padding: 40px;
      border-radius: 20px;
      box-shadow: 0 12px 24px rgba(0, 0, 0, 0.08);
      text-align: center;
      max-width: 600px;
      width: 100%;
      transition: transform 0.3s ease-in-out, box-shadow 0.3s ease;
  	}
  	.main-box:hover {
      transform: translateY(-5px);
      box-shadow: 0 15px 30px rgba(0, 0, 0, 0.12);
  	}
  	h1, h2, h3 {
      color: #512da8;
      font-weight: 700;
      margin-bottom: 15px;
  	}
  	p {
    	font-size: 16px;
      color: #555;
      line-height: 1.6;
      margin-bottom: 20px;
  	}
  	.btn-purple {
      background-color: #7b1fa2;
      color: #ffffff;
      border: none;
      padding: 12px 30px;
      border-radius: 12px;
      font-size: 16px;
      cursor: pointer;
      transition: background-color 0.3s ease, box-shadow 0.3s ease;
  	}
      .btn-purple:hover {
        background-color: #6a1b9a;
      box-shadow: 0 4px 12px rgba(123, 31, 162, 0.4);
  	}
  	.btn-block {
    	display: block;
    	width: 100%;
  	}
  	.navbar {
        background-color: #7b1fa2 !important;
      border-bottom: 4px solid #ba68c8;
  	}
  	.navbar-brand {
    	font-weight: bold;
      color: white !important;
  	}
  	.navbar-nav > li > a {
    	color: white !important;
      font-weight: 600;
      transition: color 0.3s ease;
  	}
  	.navbar-nav > li > a:hover {
      color: #f3e5f5 !important;
  	}
  	
  	#map {
    	height: 380px;
    	border-radius: 14px;
    	margin-top: 15px;
    	border: 1px solid #e0e0e0;
  	}
  	.dt-center {
    	text-align: center;
  	}
	"))
  ),
  
  navbarPage(
    title = div(
      img(
        src = "https://drive.google.com/thumbnail?id=1Z8LoczMonL1Udns7pQVOGA_OrUvELPmm",
        style = "height:30px; margin-right:10px; vertical-align:middle; border-radius: 6px;"
      ),
      span("Valuasi Aset Pro", style = "vertical-align:middle; font-weight:bold; color:white; font-size:18px;")
    ),
    id = "nav",
    inverse = TRUE,
    
    tabPanel("Beranda",
             div(class = "center-wrapper",
                 div(class = "main-box",
                     tags$img(src = "https://drive.google.com/thumbnail?id=1Z8LoczMonL1Udns7pQVOGA_OrUvELPmm", style = "margin-bottom: 20px; border-radius: 12px;"),
                     h1("Sistem Valuasi Lokasi Aset"),
                     p("Aplikasi modern untuk menganalisis nilai tanah berdasarkan lokasi, aksesibilitas, dan proyeksi investasi."),
                     br(),
                     actionButton("start_btn", "Masuk/ daftar", class = "btn-purple",
                                  onclick = "document.getElementById('nav').querySelector('a[data-value=\"Login\"]').click();"),
                     br(), br(),
                     fluidRow(
                       column(6,
                              actionButton("simulasi_btn_home", "Simulasi Investasi Tanah", class = "btn-purple btn-block",
                                           onclick = "Shiny.setInputValue('navtab', 'Simulasi Investasi Tanah', {priority: 'event'})")
                       ),
                       column(6,
                              actionButton("analisis_btn_home", "Analisis Pasar", class = "btn-purple btn-block",
                                           onclick = "Shiny.setInputValue('navtab', 'Analisis Pasar', {priority: 'event'})")
                       )
                     ),
                     br(),
                     fluidRow(
                       column(12,
                              actionButton("penilaian_btn_home", "Penilaian Lokasi", class = "btn-purple btn-block",
                                           onclick = "Shiny.setInputValue('navtab', 'Penilaian Lokasi', {priority: 'event'})")
                       )
                     ),
                     br(),
                     fluidRow(
                       column(12,
                              actionButton("dataharga_btn_home", "Data Harga Lengkap", class = "btn-purple btn-block",
                                           onclick = "Shiny.setInputValue('navtab', 'Data Harga Lengkap', {priority: 'event'})")
                       )
                     )
                 )
             )
    ),
    tabPanel("Login",
             div(class = "main-box",
                 h2("Login ke Valuasi Aset Pro"),
                 textInput("login_user", "Username"),
                 passwordInput("login_pass", "Password"),
                 actionButton("login_btn", "Login", class = "btn-purple"),
                 textOutput("login_msg")
             )
    ),
    
    
    tabPanel("Penilaian Lokasi",
             fluidRow(
               column(6,
                      h3("Input Data Lokasi"),
                      numericInput("luas", "Luas Tanah (m²)", value = NULL, min = 1),
                      selectInput("provinsi", "Provinsi / Kawasan", choices = unique(harga_lengkap$Provinsi_Kawasan)),
                      uiOutput("wilayah_ui"),
                      numericInput("akses", "Kepadatan Penduduk (per 1km²)", value = NULL, min = 1, max = 10000000),
                      actionButton("hitung", "Analisis Lokasi", class = "btn-purple")
               ),
               column(6,
                      h3("Hasil Valuasi"),
                      uiOutput("hasil_valuasi"),
                      leafletOutput("map", height = "350px")
               )
             )
    ),
    
    tabPanel("Simulasi Investasi Tanah",
             fluidRow(
               column(6,
                      h3("Input Simulasi"),
                      numericInput("investasi_awal", "Harga Perolehan Awal (Rp)", value = NULL, min = 1),
                      numericInput("tahun_proyeksi", "Proyeksi Tahun", value = 0, min = 1, max = 20),
                      numericInput("pertumbuhan", "Pertumbuhan Harga (%) (Berdasarkan yang berlaku dipasaran)", value = 0, min = 0, max = 50),
                      actionButton("simulasi_btn", "Jalankan Simulasi", class = "btn-purple")
               ),
               column(6,
                      h3("Hasil Simulasi"),
                      plotlyOutput("grafik_proyeksi")
               )
             )
    ),
    
    tabPanel("Analisis Pasar",
             fluidRow(
               column(6,
                      h3("Grafik Harga Zona"),
                      plotlyOutput("grafik_harga_zona")
               ),
               column(6,
                      h3("Rekomendasi Investasi"),
                      uiOutput("rekomendasi_pasar")
               )
             )
    ),
    
    tabPanel("Data Harga Lengkap",
             fluidRow(
               column(12,
                      h3("Tabel Harga Tanah Lengkap"),
                      DTOutput("harga_lengkap_table")
               )
             )
    )
  )
)


server <- function(input, output, session) {
  login_status <- reactiveValues(logged_in = FALSE)
  
  observeEvent(input$login_btn, {
    user_row <- user[user$username == input$login_user & user$password == input$login_pass, ]
    
    if (nrow(user_row) == 1) {
      login_status$logged_in <- TRUE
      updateTabsetPanel(session, "nav", selected = "Penilaian Lokasi")
    } else {
      output$login_msg <- renderText("❌ Username atau Password salah.")
    }
  })
  
  protected_ui <- function(ui_content){
    if(login_status$logged_in) {
      ui_content
    } else {
      div(class = "main-box",
          h3("🔒 Anda harus login terlebih dahulu."),
          actionButton("redirect_login", "Login Sekarang", class = "btn-purple",
                       onclick = "document.getElementById('nav').querySelector('a[data-value=\"Login\"]').click();")
      )
    }
  }
  
  output$main_content <- renderUI({
    if (login_status$logged_in) {
      fluidRow(
        column(6, h3("Input Data Lokasi")),
        column(6, h3("Hasil Valuasi"))
      )
    } else {
      div(class = "main-box", h3("Silakan login terlebih dahulu."))
    }
  })
  
  # === UI: Dropdown wilayah dinamis ===
  output$wilayah_ui <- renderUI({
    req(input$provinsi)
    wilayah_choices <- harga_lengkap %>%
      filter(Provinsi_Kawasan == input$provinsi) %>%
      distinct(Wilayah_Lokasi) %>%
      pull()
    
    selectInput("wilayah", "Wilayah / Lokasi", choices = wilayah_choices)
  })
  
  # === Fungsi: Hitung Valuasi ===
  hitung_valuasi <- function(luas, provinsi, wilayah, akses) {
    ref <- harga_lengkap %>%
      filter(Provinsi_Kawasan == provinsi, Wilayah_Lokasi == wilayah)
    
    if (nrow(ref) == 0) return(NULL)
    
    harga_rata <- (ref$Harga_Min[1] + ref$Harga_Max[1]) / 2
    
    zona_skor <- case_when(
      grepl("Komersial|Jakarta|Bali", provinsi, ignore.case = TRUE) ~ 10,
      grepl("Yogyakarta|Jawa Barat|Jawa Tengah|Jawa Timur", provinsi, ignore.case = TRUE) ~ 7,
      TRUE ~ 5
    )
    
    total_skor <- round(zona_skor * 0.4 + akses * 0.3 + (harga_rata / 1e6) * 0.3, 2)
    harga_jual <- round(luas * harga_rata * (total_skor / 10), 0)
    
    kategori_potensi <- if (total_skor >= 1000) {
      "Tinggi"
    } else if (total_skor <= 100) {
      "Rendah"
    } else if (total_skor < 1000) {
      "Sedang"
    } else {
      "Sangat Tinggi"
    }
    
    rekomendasi <- switch(kategori_potensi,
                          "Sangat Tinggi" = "Lokasi sangat strategis. Sangat direkomendasikan untuk investasi.",
                          "Tinggi"    	= "Lokasi berpotensi dan layak",
                          "Sedang"    	= "Lokasi berpotensi cukup baik. Perlu evaluasi lebih lanjut.",
                          "Rendah"    	= "Lokasi kurang menjanjikan. Disarankan mencari alternatif lain."
    )
    
    list(
      total_skor = total_skor,
      harga_jual = harga_jual,
      kategori_potensi = kategori_potensi,
      rekomendasi = rekomendasi,
      harga_min = ref$Harga_Min[1],
      harga_max = ref$Harga_Max[1],
      lat = ref$lat[1],
      lng = ref$lng[1]
    )
  }
  
  # === Event: Analisis Lokasi ===
  observeEvent(input$hitung, {
    req(input$luas, input$provinsi, input$wilayah, input$akses)
    
    hasil <- hitung_valuasi(input$luas, input$provinsi, input$wilayah, input$akses)
    
    if (is.null(hasil)) {
      output$hasil_valuasi <- renderUI({
        div(class = "alert alert-danger", "Data lokasi tidak ditemukan. Silakan pilih lokasi lain.")
      })
      leafletProxy("map") %>% clearMarkers()
      return()
    }
    
    output$hasil_valuasi <- renderUI({
      div(style = "background-color:#f3e5f5; border-radius:10px; padding:20px; margin-top:15px;",
          h4("Hasil Analisis Lokasi"),
          tags$ul(
            tags$li(strong("Total Skor Lokasi:"), paste(hasil$total_skor)),
            tags$li(strong("Estimasi Harga Jual:"), paste0("Rp ", formatC(hasil$harga_jual, format = "f", big.mark = ".", digits = 0))),
            tags$li(strong("Kategori Potensi:"), hasil$kategori_potensi),
            tags$li(strong("Rentang Harga Referensi:"),
                    paste0("Rp ", formatC(hasil$harga_min, format = "f", big.mark = ".", digits = 0),
                           " - Rp ", formatC(hasil$harga_max, format = "f", big.mark = ".", digits = 0)))
          ),
          p(strong("Rekomendasi:"), hasil$rekomendasi)
      )
    })
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng = hasil$lng, lat = hasil$lat, zoom = 13) %>%
      addMarkers(
        lng = hasil$lng, lat = hasil$lat,
        popup = paste0(
          "<b>", input$wilayah, ", ", input$provinsi, "</b><br/>",
          "Harga: Rp ", format(hasil$harga_min, big.mark = "."),
          " - Rp ", format(hasil$harga_max, big.mark = "."), "<br/>",
          "<i>Kategori: ", hasil$kategori_potensi, "</i>"
        )
      )
  })
  
  # === Simulasi Investasi ===
  simulasi_investasi <- reactive({
    req(input$investasi_awal, input$tahun_proyeksi, input$pertumbuhan)
    tahun <- 0:input$tahun_proyeksi
    nilai <- input$investasi_awal * (1 + input$pertumbuhan/100)^tahun
    data.frame(Tahun = tahun, Nilai = round(nilai, 0))
  })
  
  output$grafik_proyeksi <- renderPlotly({
    req(input$simulasi_btn)
    plot_ly(simulasi_investasi(), x = ~Tahun, y = ~Nilai,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#7e57c2', width = 3),
            marker = list(size = 8)) %>%
      layout(
        title = "Proyeksi Pertumbuhan Nilai Investasi",
        xaxis = list(title = "Tahun"),
        yaxis = list(title = "Nilai Investasi (Rp)")
      )
  })
  
  # === Grafik Perbandingan Harga ===
  output$grafik_harga_zona <- renderPlotly({
    harga_ringkasan <- harga_lengkap %>%
      group_by(Provinsi_Kawasan) %>%
      summarise(harga_min = mean(Harga_Min), harga_max = mean(Harga_Max), .groups = 'drop')
    
    plot_ly(harga_ringkasan, x = ~Provinsi_Kawasan, y = ~harga_min,
            type = 'bar', name = 'Harga Min', marker = list(color = '#7e57c2')) %>%
      add_trace(y = ~harga_max, name = 'Harga Maks', marker = list(color = '#d1c4e9')) %>%
      layout(
        title = "Perbandingan Harga Rata-rata Tanah per Provinsi/Kawasan",
        xaxis = list(title = "Provinsi/Kawasan"),
        yaxis = list(title = "Harga per m² (Rp)"),
        barmode = 'group'
      )
  })
  
  # === Rekomendasi Investasi Pasar ===
  output$rekomendasi_pasar <- renderUI({
    div(style = "padding:15px; background:#f3e5f5; border-radius:8px;",
        p("Berikut beberapa rekomendasi berdasarkan tren pasar:"),
        tags$ul(
          tags$li("Perhatikan lokasi & akses untuk menilai potensi."),
          tags$li("Zona komersial cenderung lebih stabil dan menguntungkan."),
          tags$li("Wilayah suburban bisa menawarkan nilai jangka panjang.")
        ),
        p(strong("Catatan:"), "Selalu lakukan due diligence sebelum keputusan investasi.")
    )
  })
  
  # === Tabel Harga Lengkap ===
  output$harga_lengkap_table <- renderDT({
    datatable(
      harga_lengkap %>%
        rename(
          "Provinsi / Kawasan" = Provinsi_Kawasan,
          "Wilayah / Lokasi" = Wilayah_Lokasi,
          "Harga Min (Rp)" = Harga_Min,
          "Harga Max (Rp)" = Harga_Max
        ),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE),
      class = "stripe hover compact"
    ) %>%
      formatCurrency(c("Harga Min (Rp)", "Harga Max (Rp)"), currency = "Rp ", digits = 0, interval = 3, mark = ".", dec.mark = ",")
  })
  
  # === Peta Awal ===
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 110, lat = -6, zoom = 5)
  })
  
  # === Navigasi Shortcut dari Beranda ===
  observeEvent(input$navtab, {
    updateTabsetPanel(session, "nav", selected = input$navtab)
  })
}

shinyApp(ui = ui, server = server)




