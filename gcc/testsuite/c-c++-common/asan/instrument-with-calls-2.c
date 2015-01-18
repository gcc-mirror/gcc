/* { dg-do compile } */
/* { dg-options "--param asan-instrumentation-with-call-threshold=1 -ffat-lto-objects" } */

int x;

void f(int *a, int *b) {
  *a = 0;
  asm volatile ("" ::: "memory");
  x = *b;
}

/* { dg-final { scan-assembler "__asan_store4" } } */
/* { dg-final { scan-assembler-not "__asan_report_store4" } } */
/* { dg-final { scan-assembler "__asan_load4" } } */
/* { dg-final { scan-assembler-not "__asan_report_load4" } } */
