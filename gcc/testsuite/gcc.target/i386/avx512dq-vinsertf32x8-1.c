/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vinsertf32x8\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vinsertf32x8\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vinsertf32x8\[ \\t\]+\[^\{\n\]*(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m256 y;

void extern
avx512dq_test (void)
{
  x = _mm512_insertf32x8 (x, y, 1);
  x = _mm512_mask_insertf32x8 (x, 2, x, y, 1);
  x = _mm512_maskz_insertf32x8 (2, x, y, 1);
}
