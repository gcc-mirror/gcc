*cpp_options:
%(cpp_unique_options) %1 %{m*} %{std*&ansi&trigraphs} %{W*&pedantic*} %{w}\
 %{f*} %{g*:%{!g0:%{g*} %{!fno-working-directory:-fworking-directory}}} %{O*}\
 %{undef} %{save-temps*:-fpch-preprocess} %{D_TEST_D:-D_FOO}

