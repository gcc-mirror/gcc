/* { dg-do assemble } */
/* { dg-options "-fno-sanitize=address -fsanitize=kernel-address --param asan-instrumentation-with-call-threshold=0 -save-temps" } */

void f(char *a, int *b) {
  *b = *a;
}

/* { dg-final { scan-assembler "__asan_load1" } } */
/* { dg-final { scan-assembler "__asan_store4" } } */
/* { dg-final { cleanup-saved-temps } } */
