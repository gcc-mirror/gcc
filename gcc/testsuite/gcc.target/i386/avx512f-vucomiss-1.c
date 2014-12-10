/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vucomiss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  } } */

#include <immintrin.h>

volatile __m128 x;
volatile int res;

void extern
avx512f_test (void)
{
  res = _mm_comi_round_ss (x, x, _CMP_EQ_OQ, _MM_FROUND_NO_EXC);
}
