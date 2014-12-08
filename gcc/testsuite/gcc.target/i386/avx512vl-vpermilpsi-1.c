/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 y;
volatile __m128 x;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  y = _mm256_mask_permute_ps (y, m, y, 13);
  y = _mm256_maskz_permute_ps (m, y, 13);
  x = _mm_mask_permute_ps (x, m, x, 13);
  x = _mm_maskz_permute_ps (m, x, 13);
}
