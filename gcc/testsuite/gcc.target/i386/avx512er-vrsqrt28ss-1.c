/* { dg-do compile } */
/* { dg-options "-mavx512er -O2" } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x, y;

void extern
avx512er_test (void)
{
  x = _mm_rsqrt28_ss (x, y);
  x = _mm_rsqrt28_round_ss (x, y, _MM_FROUND_NO_EXC);
}
