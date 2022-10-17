/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i res1;
volatile __m128i res2;
volatile __m128h x3;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res1 = _mm256_cvttph_epu32 (x3);
  res1 = _mm256_mask_cvttph_epu32 (res1, m8, x3);
  res1 = _mm256_maskz_cvttph_epu32 (m8, x3);

  res2 = _mm_cvttph_epu32 (x3);
  res2 = _mm_mask_cvttph_epu32 (res2, m8, x3);
  res2 = _mm_maskz_cvttph_epu32 (m8, x3);
}
