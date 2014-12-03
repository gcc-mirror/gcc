/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovdb\[ \\t\]+\[^\{\n\]*(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmovdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m128i res;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtepi32_epi8 (s);
  res = _mm512_mask_cvtepi32_epi8 (res, m, s);
  res = _mm512_maskz_cvtepi32_epi8 (m, s);
  _mm512_mask_cvtepi32_storeu_epi8 ((void *) &res, m, s);
}
