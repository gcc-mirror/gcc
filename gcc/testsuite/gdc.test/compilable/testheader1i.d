/*
EXTRA_SOURCES: extra-files/header1.d
REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheader1i.di -inline -ignore
PERMUTE_ARGS: -d -dw
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheader1i.di
TEST_OUTPUT_FILE: extra-files/header1i.di
*/

void main() {}
