/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovusdb\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusdb\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusdb\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m128i res;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtusepi32_epi8 (s);
  res = _mm512_mask_cvtusepi32_epi8 (res, m, s);
  res = _mm512_maskz_cvtusepi32_epi8 (m, s);
}
