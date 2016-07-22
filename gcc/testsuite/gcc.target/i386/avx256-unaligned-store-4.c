/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mno-avx256-split-unaligned-load -mno-avx256-split-unaligned-store -mno-prefer-avx128 -fno-common" } */

#define N 1024

extern float a[N], b[N+3], c[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * c[i];
}

/* { dg-final { scan-assembler "vmovups.*movv8sf_internal/4" } } */
/* { dg-final { scan-assembler-not "movups.*movv4sf_internal/4" } } */
/* { dg-final { scan-assembler-not "vextractf128" } } */
