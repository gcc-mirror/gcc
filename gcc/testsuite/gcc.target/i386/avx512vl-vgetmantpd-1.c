/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x, y;
volatile __m128d a, b;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_getmant_pd (y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm256_mask_getmant_pd (x, m, y, _MM_MANT_NORM_p75_1p5,
			      _MM_MANT_SIGN_src);
  x = _mm256_maskz_getmant_pd (m, y, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  a = _mm_getmant_pd (b, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  a = _mm_mask_getmant_pd (a, m, b, _MM_MANT_NORM_p75_1p5,
			   _MM_MANT_SIGN_src);
  a = _mm_maskz_getmant_pd (m, b, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}
