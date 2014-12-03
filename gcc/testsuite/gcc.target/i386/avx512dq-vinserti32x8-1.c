/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vinserti32x8\[ \\t\]+\[^\{\n\]*(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti32x8\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vinserti32x8\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;

void extern
avx512dq_test (void)
{
  x = _mm512_inserti32x8 (x, y, 1);
  x = _mm512_mask_inserti32x8 (x, 2, x, y, 1);
  x = _mm512_maskz_inserti32x8 (2, x, y, 1);
}
