/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -O2 -ftree-vectorize  -ffixed-xmm0 -ffixed-xmm1 -ffixed-xmm2 -ffixed-xmm3 -ffixed-xmm4 -ffixed-xmm5 -ffixed-xmm6 -ffixed-xmm7 -ffixed-xmm8 -ffixed-xmm9 -ffixed-xmm10 -ffixed-xmm11 -ffixed-xmm12 -ffixed-xmm13 -ffixed-xmm14" } */
/* { dg-final { scan-assembler-times "vbroadcastsd\[ \\t\]+(?:%xmm(?:\[0-9\]|1\[0-5\]),\[ \\t\]*%ymm(?:\[0-9\]|1\[0-5\])|%xmm\[0-9\]+,\[ \\t\]*%zmm)" 1 } } */

#include <immintrin.h>

register __m512d z asm ("zmm16"); /* { dg-warning "call-clobbered register used for global register variable" } */

double a[10000];

void foo (unsigned N)
{
  double d;
  _mm_store_sd(&d,  _mm256_extractf128_pd (_mm512_extractf64x4_pd (z, 0), 0));

  for (int i=0; i<N; i++)
    {
      a[i] = d;
      a[i] += a[i-4];
    }
}
