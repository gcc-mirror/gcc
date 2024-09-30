/* { dg-do compile } */
/* { dg-options "-O0 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i y;
volatile __m128i x;
volatile __mmask32 m2;
volatile __mmask16 m3;

void extern
avx512bw_test (void)
{
  y = _mm256_mask_alignr_epi8 (y, m2, y, y, 10);
  x = _mm_mask_alignr_epi8 (x, m3, x, x, 10);
}
