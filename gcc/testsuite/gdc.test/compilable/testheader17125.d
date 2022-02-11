/*
EXTRA_SOURCES: extra-files/header17125.d
PERMUTE_ARGS:
REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheader17125.di
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheader17125.di

TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/testheader17125.di
// D import file generated from 'compilable/extra-files/header17125.d'
void func1(real value = 103500.0L);
void func2(real value = 520199.0F);
void func3(real value = 970000.0);
void func4(real value = 102450.0F);
void func5(real value = 412502.0L);
---
*/

void main() {}
