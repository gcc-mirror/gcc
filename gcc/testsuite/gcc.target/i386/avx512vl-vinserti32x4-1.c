/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\]*ymm" 3 } } */
/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\]*\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vinserti32x4\[^\n\]*\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i y;

void extern
avx512vl_test (void)
{
  x = _mm256_inserti32x4 (x, y, 1);
  x = _mm256_mask_inserti32x4 (x, 2, x, y, 1);
  x = _mm256_maskz_inserti32x4 (2, x, y, 1);
}
