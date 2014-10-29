/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vextracti32x8\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextracti32x8\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vextracti32x8\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;

void extern
avx512dq_test (void)
{
  y = _mm512_extracti32x8_epi32 (x, 1);
  y = _mm512_mask_extracti32x8_epi32 (y, 2, x, 1);
  y = _mm512_maskz_extracti32x8_epi32 (2, x, 1);
}
