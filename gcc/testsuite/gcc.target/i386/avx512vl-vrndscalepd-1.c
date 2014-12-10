/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  3 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  3 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x1;
volatile __m128d x2;

void extern
avx512vl_test (void)
{
  x1 = _mm256_roundscale_pd (x1, 0x42);
  x1 = _mm256_ceil_pd (x1);
  x1 = _mm256_floor_pd (x1);
  x1 = _mm256_mask_roundscale_pd (x1, 2, x1, 0x42);
  x1 = _mm256_maskz_roundscale_pd (2, x1, 0x42);
  x2 = _mm_roundscale_pd (x2, 0x42);
  x2 = _mm_ceil_pd (x2);
  x2 = _mm_floor_pd (x2);
  x2 = _mm_mask_roundscale_pd (x2, 2, x2, 0x42);
  x2 = _mm_maskz_roundscale_pd (2, x2, 0x42);
}
