/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store -mno-prefer-avx128 -fno-common" } */

#define N 1024

extern float a[N], b[N+3], c[N], d[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * 10.0;

  for (i = 0; i < N; i++)
    d[i] = c[i] * 20.0;
}

/* { dg-final { scan-assembler-not "vmovups.*movv8sf_internal/4" } } */
/* { dg-final { scan-assembler "vmovups.*movv4sf_internal/4" } } */
/* { dg-final { scan-assembler "vextractf128" } } */
