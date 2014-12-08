/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "(?:vblendmpd|vmovapd)\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vblendmpd|vmovapd)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x;
volatile __m128d xx;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_mask_blend_pd (m, x, x);
  xx = _mm_mask_blend_pd (m, xx, xx);
}
