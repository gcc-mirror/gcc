/* { dg-do compile } */
/* { dg-options "--param asan-instrumentation-with-call-threshold=0 -ffat-lto-objects" } */

void f(char *a, int *b) {
  *b = *a;
}

/* { dg-final { scan-assembler "__asan_load1" } } */
/* { dg-final { scan-assembler "__asan_store4" } } */
