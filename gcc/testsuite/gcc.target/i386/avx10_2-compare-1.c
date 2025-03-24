/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vcomxsd\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  } } */
/* { dg-final { scan-assembler-times "vcomxss\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vucomxsd\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  } } */
/* { dg-final { scan-assembler-times "vucomxss\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  } } */

#include <immintrin.h>

volatile __m128 x3;
volatile __m128d x4;
volatile int a;

void extern
avx10_2_test (void)
{
  a = _mm_comi_round_sd (x4, x4, _CMP_EQ_OS, _MM_FROUND_NO_EXC);
  a = _mm_comi_round_ss (x3, x3, _CMP_NEQ_US, _MM_FROUND_NO_EXC);
  a = _mm_comi_round_sd (x4, x4, _CMP_EQ_OQ, _MM_FROUND_NO_EXC);
  a = _mm_comi_round_ss (x3, x3, _CMP_NEQ_UQ, _MM_FROUND_NO_EXC);
}
