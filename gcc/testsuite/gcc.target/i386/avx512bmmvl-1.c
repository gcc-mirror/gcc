/* { dg-do compile } */
/* { dg-options "-mavx512bmm -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vbmacor16x16x16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbmacxor16x16x16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m256i x,y,z;
volatile __m128i x_,y_,z_;
volatile __mmask32 m;
volatile __mmask16 m_;

void extern
avx512bmm_test (void)
{
  x = _mm256_bmacor16x16x16 (x, y, z);

  x = _mm256_bmacxor16x16x16 (x, y, z);

  x = _mm256_mask_bitrev_epi8 (m, x, y);
  x_ = _mm128_mask_bitrev_epi8 (m_, x_, y_);

  x = _mm256_maskz_bitrev_epi8 (m, y);
  x_ = _mm128_maskz_bitrev_epi8 (m_, y_);

  x = _mm256_bitrev_epi8 (x);
  x_ = _mm128_bitrev_epi8 (x_);
}
