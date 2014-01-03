/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vextractf32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vextractf32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vextractf32x4\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m128 y;

void extern
avx512f_test (void)
{
  y = _mm512_extractf32x4_ps (x, 1);
  y = _mm512_mask_extractf32x4_ps (y, 2, x, 1);
  y = _mm512_maskz_extractf32x4_ps (2, x, 1);
}
