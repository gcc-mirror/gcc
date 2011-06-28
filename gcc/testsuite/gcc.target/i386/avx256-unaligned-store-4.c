/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mno-avx256-split-unaligned-load -mno-avx256-split-unaligned-store" } */

#define N 1024

float a[N], b[N+3], c[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * c[i];
}

/* { dg-final { scan-assembler "\\*avx_movups256/2" } } */
/* { dg-final { scan-assembler-not "\\*avx_movups/2" } } */
/* { dg-final { scan-assembler-not "\\*avx_movv4sf_internal/3" } } */
/* { dg-final { scan-assembler-not "vextractf128" } } */
