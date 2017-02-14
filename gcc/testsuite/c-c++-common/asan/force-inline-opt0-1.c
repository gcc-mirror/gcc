/* This test checks that we are no instrumenting a memory access twice
   (before and after inlining) */

/* { dg-do compile } */
/* { dg-options "-fno-sanitize-address-use-after-scope" } */
/* { dg-final { scan-assembler-not "__asan_report_load" } } */

__attribute__((always_inline))
inline void foo(int *x) {
  *x = 0;
}

int main() {
  int x;
  foo(&x);
  return x;
}
