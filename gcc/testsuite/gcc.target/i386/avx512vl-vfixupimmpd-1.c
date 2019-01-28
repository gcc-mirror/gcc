/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d xx;
volatile __m256i yy;
volatile __m128d x2;
volatile __m128i y2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  xx = _mm256_fixupimm_pd (xx, xx, yy, 3);
  xx = _mm256_mask_fixupimm_pd (xx, m, xx, yy, 3);
  xx = _mm256_maskz_fixupimm_pd (m, xx, xx, yy, 3);
  x2 = _mm_fixupimm_pd (x2, x2, y2, 3);
  x2 = _mm_mask_fixupimm_pd (x2, m, x2, y2, 3);
  x2 = _mm_maskz_fixupimm_pd (m, x2, x2, y2, 3);
}
