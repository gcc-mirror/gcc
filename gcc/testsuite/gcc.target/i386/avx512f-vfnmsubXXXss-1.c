/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vfnmsub...ss\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128 a, b, c;

void extern
avx512f_test (void)
{
  a = _mm_fnmsub_round_ss (a, b, c, _MM_FROUND_TO_NEAREST_INT);
}
