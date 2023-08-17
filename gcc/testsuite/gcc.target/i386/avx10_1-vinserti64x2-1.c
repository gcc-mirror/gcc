/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i y;

void extern
avx10_1_test (void)
{
  x = _mm256_inserti64x2 (x, y, 1);
  x = _mm256_mask_inserti64x2 (x, 2, x, y, 1);
  x = _mm256_maskz_inserti64x2 (2, x, y, 1);
}
