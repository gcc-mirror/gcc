/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */

#include <immintrin.h>

volatile __m256 xx;
volatile __m128 x2;
volatile __mmask8 m8;

void extern
avx512vl_test (void)
{
  xx = _mm256_getexp_ps (xx);
  xx = _mm256_mask_getexp_ps (xx, m8, xx);
  xx = _mm256_maskz_getexp_ps (m8, xx);
  x2 = _mm_getexp_ps (x2);
  x2 = _mm_mask_getexp_ps (x2, m8, x2);
  x2 = _mm_maskz_getexp_ps (m8, x2);
}
