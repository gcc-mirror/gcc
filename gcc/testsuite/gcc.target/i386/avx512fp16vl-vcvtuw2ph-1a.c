/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256h res2;
volatile __m128h res3;
volatile __m256i x2;
volatile __m128i x3;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res2 = _mm256_cvtepu16_ph (x2);
  res2 = _mm256_mask_cvtepu16_ph (res2, m16, x2);
  res2 = _mm256_maskz_cvtepu16_ph (m16, x2);

  res3 = _mm_cvtepu16_ph (x3);
  res3 = _mm_mask_cvtepu16_ph (res3, m8, x3);
  res3 = _mm_maskz_cvtepu16_ph (m8, x3);
}
