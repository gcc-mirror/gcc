/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vinserti64x4\[ \\t\]+\[^\n\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vinserti64x4\[ \\t\]+\[^\n\]+\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vinserti64x4\[ \\t\]+\[^\n\]+\[^\n\]" 3 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;

void extern
avx512f_test (void)
{
  x = _mm512_inserti64x4 (x, y, 1);
  x = _mm512_mask_inserti64x4 (x, 2, x, y, 1);
  x = _mm512_maskz_inserti64x4 (2, x, y, 1);
}
