/* { dg-do link } */
/* { dg-require-effective-target ptr32plus } */
/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */
/* This validates codegen for [r1+32760] on Darwin. */
void f(char x[32688], double *y, double *z) __attribute__((noinline));
void f(char x[32688], double *y, double *z) {}
main() {
  char x[32688];
  double y, z;
  y = z = 3.0;
  f(x, &y, &z);
}
