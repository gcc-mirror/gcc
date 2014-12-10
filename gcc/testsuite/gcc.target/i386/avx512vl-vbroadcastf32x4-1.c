/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}|vshuff32x4\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128 y;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_broadcast_f32x4 (y);
  x = _mm256_mask_broadcast_f32x4 (x, m, y);
  x = _mm256_maskz_broadcast_f32x4 (m, y);
}
