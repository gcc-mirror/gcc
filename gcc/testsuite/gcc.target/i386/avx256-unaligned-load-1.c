/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load -mno-prefer-avx128" } */

#define N 1024

float a[N], b[N+3], c[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = a[i] * b[i+3];
}

/* { dg-final { scan-assembler-not "vmovups\[^\n\r]*movv8sf_internal/3" } } */
/* { dg-final { scan-assembler "movv4sf_internal/3" } } */
/* { dg-final { scan-assembler "vinsertf128" } } */
