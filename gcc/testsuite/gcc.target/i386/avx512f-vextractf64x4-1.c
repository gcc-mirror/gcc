/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vextractf64x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextractf64x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vextractf64x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m256d y;

void extern
avx512f_test (void)
{
  y = _mm512_extractf64x4_pd (x, 1);
  y = _mm512_maskz_extractf64x4_pd (2, x, 1);
  y = _mm512_mask_extractf64x4_pd (y, 2, x, 1);
}
