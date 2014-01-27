/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovusqw\[ \\t\]+\[^\n\]*" 4 } } */
/* { dg-final { scan-assembler-times "vpmovusqw\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqw\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqw\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m128i res;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtusepi64_epi16 (s);
  res = _mm512_mask_cvtusepi64_epi16 (res, m, s);
  res = _mm512_maskz_cvtusepi64_epi16 (m, s);
  _mm512_mask_cvtusepi64_storeu_epi16 ((void *) &res, m, s);
}
