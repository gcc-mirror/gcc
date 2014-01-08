/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vrndscaless\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vrndscaless\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128 x1, x2;

void extern
avx512f_test (void)
{
  x1 = _mm_roundscale_ss (x1, x2, 0x42);
  x1 = _mm_roundscale_round_ss (x1, x2, 0x42, _MM_FROUND_NO_EXC);
}
