/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovqw\[ \\t\]+\[^\{\n\]*(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmovqw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovqw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovqw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m128i res;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtepi64_epi16 (s);
  res = _mm512_mask_cvtepi64_epi16 (res, m, s);
  res = _mm512_maskz_cvtepi64_epi16 (m, s);
  _mm512_mask_cvtepi64_storeu_epi16 ((void *) &res, m, s);
}
