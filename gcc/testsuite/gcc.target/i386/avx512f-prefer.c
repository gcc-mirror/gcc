/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512 -mprefer-vector-width=256" } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "vmulpd" } } */

#define N 1024

double a[N], b[N], c[N];

void
avx512f_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = a[i] * b[i];
}

