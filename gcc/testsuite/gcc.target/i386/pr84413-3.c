/* { dg-do compile } */
/* { dg-options "-O3 -march=icelake-server" } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "vmulpd\[ \\t\]+\[^\n\]*%ymm\[0-9\]+" } } */

#define N 1024

double a[N], b[N], c[N];

void
avx512f_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = a[i] * b[i];
}
