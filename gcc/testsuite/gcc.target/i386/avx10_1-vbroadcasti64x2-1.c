/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vbroadcasti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\]|vshufi64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcasti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\[^\{\]|vshufi64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcasti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}|vshufi64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i y;
volatile __m128i x;
volatile __mmask8 m;

void extern
avx10_1_test (void)
{
  y = _mm256_broadcast_i64x2 (x);
  y = _mm256_mask_broadcast_i64x2 (y, m, x);
  y = _mm256_maskz_broadcast_i64x2 (m, x);
}
