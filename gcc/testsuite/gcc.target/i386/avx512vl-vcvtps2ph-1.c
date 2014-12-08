/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcvtps2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2ph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2ph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128i y;
volatile __m128 xx;
volatile __m128i yy;

void extern
avx512bw_test (void)
{
  y = _mm256_maskz_cvtps_ph (4, x, 0);
  y = _mm256_mask_cvtps_ph (y, 2, x, 0);
  yy = _mm_maskz_cvtps_ph (4, xx, 0);
  yy = _mm_mask_cvtps_ph (yy, 2, xx, 0);
}
