/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "(?:vblendmps|vmovaps)\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vblendmps|vmovaps)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128 xx;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_mask_blend_ps (m, x, x);
  xx = _mm_mask_blend_ps (m, xx, xx);
}
