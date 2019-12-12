/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 res1;
volatile __m256bh x1, x2;
volatile __m128 res2;
volatile __m128bh x3, x4;
volatile __mmask8 m8;

void extern
avx512bf16_test (void)
{
  res1 = _mm256_dpbf16_ps (res1, x1, x2);
  res1 = _mm256_mask_dpbf16_ps (res1, m8, x1, x2);
  res1 = _mm256_maskz_dpbf16_ps (m8, res1, x1, x2);

  res2 = _mm_dpbf16_ps (res2, x3, x4);
  res2 = _mm_mask_dpbf16_ps (res2, m8, x3, x4);
  res2 = _mm_maskz_dpbf16_ps (m8, res2, x3, x4);
}
