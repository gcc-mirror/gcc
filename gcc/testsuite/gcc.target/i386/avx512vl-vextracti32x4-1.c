/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i y;

void extern
avx512vl_test (void)
{
  y = _mm256_extracti32x4_epi32 (x, 1);
  y = _mm256_mask_extracti32x4_epi32 (y, 2, x, 1);
  y = _mm256_maskz_extracti32x4_epi32 (2, x, 1);
}
