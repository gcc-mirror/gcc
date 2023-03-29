/* { dg-do compile } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store -mtune=generic -fno-common -mno-avx512f" } */

#define N 1024

extern double a[N], b[N+3], c[N], d[N];

void
avx_test (void)
{
  int i;

  for (i = 0; i < N; i++)
    b[i+3] = a[i] * 10.0;

  for (i = 0; i < N; i++)
    d[i] = c[i] * 20.0;
}

/* { dg-final { scan-assembler-not "vmovupd.*movv4df_internal/4" } } */
/* { dg-final { scan-assembler "vmovupd.*movv2df_internal/4" } } */
/* { dg-final { scan-assembler "vextractf128" } } */
