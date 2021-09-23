/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */

#include <immintrin.h>

volatile __m256h xx;
volatile __m128h x2;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512vl_test (void)
{
  xx = _mm256_getexp_ph (xx);
  xx = _mm256_mask_getexp_ph (xx, m16, xx);
  xx = _mm256_maskz_getexp_ph (m16, xx);
  x2 = _mm_getexp_ph (x2);
  x2 = _mm_mask_getexp_ph (x2, m8, x2);
  x2 = _mm_maskz_getexp_ph (m8, x2);
}
