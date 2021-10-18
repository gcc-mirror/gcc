/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512fp16 " } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256h x, y;
volatile __m128h a, b;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512vl_test (void)
{
  x = _mm256_getmant_ph (y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm256_mask_getmant_ph (x, m16, y, _MM_MANT_NORM_p75_1p5,
			      _MM_MANT_SIGN_src);
  x = _mm256_maskz_getmant_ph (m16, y, _MM_MANT_NORM_p75_1p5,
			       _MM_MANT_SIGN_src);
  a = _mm_getmant_ph (b, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  a = _mm_mask_getmant_ph (a, m8, b, _MM_MANT_NORM_p75_1p5,
			   _MM_MANT_SIGN_src);
  a = _mm_maskz_getmant_ph (m8, b, _MM_MANT_NORM_p75_1p5,
			    _MM_MANT_SIGN_src);
}
