/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i y, y2, y3;
volatile __m128i x, x2, x3;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  y = _mm256_ternarylogic_epi32 (y, y2, y3, 0xF0);
  y = _mm256_mask_ternarylogic_epi32 (y, m, y2, y3, 0xF0);
  y = _mm256_maskz_ternarylogic_epi32 (m, y, y2, y3, 0xF0);

  x = _mm_ternarylogic_epi32 (x, x2, x3, 0xF0);
  x = _mm_mask_ternarylogic_epi32 (x, m, x2, x3, 0xF0);
  x = _mm_maskz_ternarylogic_epi32 (m, x, x2, x3, 0xF0);
}
