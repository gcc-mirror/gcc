/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 y;
volatile __m128 x;
volatile __mmask8 m;

void extern
avx10_1_test (void)
{
  y = _mm256_range_ps (y, y, 15);
  x = _mm_range_ps (x, x, 15);

  y = _mm256_mask_range_ps (y, m, y, y, 15);
  x = _mm_mask_range_ps (x, m, x, x, 15);

  y = _mm256_maskz_range_ps (m, y, y, 15);
  x = _mm_maskz_range_ps (m, x, x, 15);
}
