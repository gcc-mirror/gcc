/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpslld\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __m256i x256;
volatile __m128i x128;
volatile __mmask8 m256;
volatile __mmask8 m128;
#define y 7

void extern
avx512vl_test (void)
{
  x256 = _mm256_mask_slli_epi32 (x256, m256, x256, y);
  x256 = _mm256_maskz_slli_epi32 (m256, x256, y);
  x128 = _mm_mask_slli_epi32 (x128, m128, x128, y);
  x128 = _mm_maskz_slli_epi32 (m128, x128, y);
}
