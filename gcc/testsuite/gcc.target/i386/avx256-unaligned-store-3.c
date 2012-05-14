/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store" } */

#define N 1024

double a[N], b[N+3], c[N], d[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * 10.0;

  for (i = 0; i < N; i++)
    d[i] = c[i] * 20.0;
}

/* { dg-final { scan-assembler-not "avx_movupd256/2" } } */
/* { dg-final { scan-assembler "movupd.*\\*avx_movv2df_internal/3" } } */
/* { dg-final { scan-assembler "vextractf128" } } */
