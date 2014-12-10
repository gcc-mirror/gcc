/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_shuffle_i32x4 (x, x, 2);
  x = _mm256_mask_shuffle_i32x4 (x, m, x, x, 2);
  x = _mm256_maskz_shuffle_i32x4 (m, x, x, 2);
}
