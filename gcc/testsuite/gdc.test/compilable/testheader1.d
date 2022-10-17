/*
EXTRA_SOURCES: extra-files/header1.d
REQUIRED_ARGS: -o- -unittest -H -Hf${RESULTS_DIR}/compilable/testheader1.di -ignore
PERMUTE_ARGS: -d -dw
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheader1.di
TEST_OUTPUT_FILE: extra-files/header1.di
*/

void main() {}
