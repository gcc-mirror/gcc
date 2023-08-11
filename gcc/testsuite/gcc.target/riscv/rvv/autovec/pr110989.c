/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=scalable -Ofast" } */

int a, b, c;
double *d;
void e() {
  double f;
  for (; c; c++, d--)
    f = *d ?: *(&a + c);
  b = f;
}
