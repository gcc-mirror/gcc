/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*ymm\[0-9]\(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*ymm\[0-9]\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*ymm\[0-9]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128 y;

void extern
avx512vl_test (void)
{
  x = _mm256_insertf32x4 (x, y, 1);
  x = _mm256_mask_insertf32x4 (x, 2, x, y, 1);
  x = _mm256_maskz_insertf32x4 (2, x, y, 1);
}
