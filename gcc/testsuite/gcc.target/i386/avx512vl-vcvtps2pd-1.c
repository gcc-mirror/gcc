/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 s;
volatile __m256d res1;
volatile __m128d res2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  res1 = _mm256_mask_cvtps_pd (res1, m, s);
  res2 = _mm_mask_cvtps_pd (res2, m, s);

  res1 = _mm256_maskz_cvtps_pd (m, s);
  res2 = _mm_maskz_cvtps_pd (m, s);
}
