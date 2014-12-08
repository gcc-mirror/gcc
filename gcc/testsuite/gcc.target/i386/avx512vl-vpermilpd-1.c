/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermilpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d y;
volatile __m256i c;
volatile __m128d x;
volatile __m128i k;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  y = _mm256_mask_permutevar_pd (y, m, y, c);
  y = _mm256_maskz_permutevar_pd (m, y, c);
  x = _mm_mask_permutevar_pd (x, m, x, k);
  x = _mm_maskz_permutevar_pd (m, x, k);
}
