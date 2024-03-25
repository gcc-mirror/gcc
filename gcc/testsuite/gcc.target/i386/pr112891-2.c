/* { dg-do compile } */
/* { dg-options "-mavx2 -O3" } */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */

void
__attribute__((noinline))
bar (double* a)
{
  a[0] = 1.0;
  a[1] = 2.0;
}

double
__attribute__((noinline))
foo (double* __restrict a, double* b)
{
  a[0] += b[0];
  a[1] += b[1];
  a[2] += b[2];
  a[3] += b[3];
  bar (b);
  return a[5] + b[5];
}

double
foo1 (double* __restrict a, double* b)
{
  double c = foo (a, b);
  return __builtin_exp (c);
}
