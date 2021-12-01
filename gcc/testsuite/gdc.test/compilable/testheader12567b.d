/*
REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheader12567b.di
PERMUTE_ARGS:
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheader12567b.di

TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/testheader12567b.di
// D import file generated from 'compilable/testheader12567b.d'
deprecated("message") module header12567b;
void main();
---
*/

deprecated("message") module header12567b;

void main() {}
