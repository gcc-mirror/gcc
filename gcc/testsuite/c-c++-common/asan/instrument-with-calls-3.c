/* { dg-do compile } */
/* { dg-options "--param asan-instrumentation-with-call-threshold=0 -ffat-lto-objects" } */

struct A {
  char x[7];
};

void f(struct A *x, struct A *y) {
  *x = *y;
}

/* { dg-final { scan-assembler "__asan_loadN" } } */
/* { dg-final { scan-assembler "__asan_storeN" } } */

