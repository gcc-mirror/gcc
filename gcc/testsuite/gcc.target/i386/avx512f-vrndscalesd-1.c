/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vrndscalesd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x1, x2;

void extern
avx512f_test (void)
{
  x1 = _mm_roundscale_sd (x1, x2, 0x42);
  x1 = _mm_roundscale_round_sd (x1, x2, 0x42, _MM_FROUND_NO_EXC);
}
