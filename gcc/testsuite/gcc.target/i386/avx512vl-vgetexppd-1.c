/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */

#include <immintrin.h>

volatile __m256d xx;
volatile __m128d x2;
volatile __mmask8 m8;

void extern
avx512vl_test (void)
{
  xx = _mm256_getexp_pd (xx);
  xx = _mm256_mask_getexp_pd (xx, m8, xx);
  xx = _mm256_maskz_getexp_pd (m8, xx);
  x2 = _mm_getexp_pd (x2);
  x2 = _mm_mask_getexp_pd (x2, m8, x2);
  x2 = _mm_maskz_getexp_pd (m8, x2);
}
