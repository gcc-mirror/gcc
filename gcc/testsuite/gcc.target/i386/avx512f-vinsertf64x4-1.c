/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vinsertf64x4\[ \\t\]+\[^\{\n\]+zmm\[0-9\](?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinsertf64x4\[ \\t\]+\[^\{\n\]+zmm\[0-9\]\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vinsertf64x4\[ \\t\]+\[^\{\n\]+zmm\[0-9\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m256d y;

void extern
avx512f_test (void)
{
  x = _mm512_insertf64x4 (x, y, 1);
  x = _mm512_mask_insertf64x4 (x, 2, x, y, 1);
  x = _mm512_maskz_insertf64x4 (2, x, y, 1);
}
