/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vextracti32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m128i y;

void extern
avx512f_test (void)
{
  y = _mm512_extracti32x4_epi32 (x, 1);
  y = _mm512_mask_extracti32x4_epi32 (y, 2, x, 1);
  y = _mm512_maskz_extracti32x4_epi32 (2, x, 1);
}
