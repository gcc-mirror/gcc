/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vextracti64x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextracti64x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vextracti64x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;

void extern
avx512f_test (void)
{
  y = _mm512_extracti64x4_epi64 (x, 1);
  y = _mm512_mask_extracti64x4_epi64 (y, 2, x, 1);
  y = _mm512_maskz_extracti64x4_epi64 (2, x, 1);
}
