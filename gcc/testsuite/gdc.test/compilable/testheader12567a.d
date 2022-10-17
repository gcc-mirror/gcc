/*
REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheader12567a.di
PERMUTE_ARGS:
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheader12567a.di

TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/testheader12567a.di
// D import file generated from 'compilable/testheader12567a.d'
deprecated module header12567a;
void main();
---
*/

deprecated module header12567a;

void main() {}
