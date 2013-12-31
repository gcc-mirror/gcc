/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcomiss\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "vcomiss\[ \\t\]+\[^{}\n\]*%xmm" 1 } } */

#include <immintrin.h>

volatile __m128 x;
volatile int res;

void extern
avx512f_test (void)
{
  res = _mm_comi_round_ss (x, x, _CMP_LT_OS, _MM_FROUND_NO_EXC);
}

void extern
avx512f_test_2 (void)
{
  res = _mm_comi_round_ss (x, x, _CMP_LT_OS, _MM_FROUND_CUR_DIRECTION);
}
