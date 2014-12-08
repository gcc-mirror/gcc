/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastsd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastsd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastsd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x;
volatile __m128d y;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_broadcastsd_pd (y);
  x = _mm256_mask_broadcastsd_pd (x, m, y);
  x = _mm256_maskz_broadcastsd_pd (m, y);
}
