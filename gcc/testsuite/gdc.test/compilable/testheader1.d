// EXTRA_SOURCES: extra-files/header1.d
// REQUIRED_ARGS: -o- -unittest -H -Hf${RESULTS_DIR}/compilable/header1.di
// PERMUTE_ARGS: -d -dw
// POST_SCRIPT: compilable/extra-files/header-postscript.sh
/*
TEST_OUTPUT:
---
Hello World
---
*/

void main() {}
