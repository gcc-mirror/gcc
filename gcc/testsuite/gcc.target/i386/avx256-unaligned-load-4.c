/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mno-avx256-split-unaligned-load -mno-avx256-split-unaligned-store" } */

#define N 1024

float a[N], b[N+3];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i] = a[i+3] * 2;
}

/* { dg-final { scan-assembler "\\*avx_movups256/1" } } */
/* { dg-final { scan-assembler-not "\\*avx_movups/1" } } */
/* { dg-final { scan-assembler-not "vinsertf128" } } */
