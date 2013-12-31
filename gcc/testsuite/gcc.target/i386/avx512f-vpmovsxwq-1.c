/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovsxwq\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwq\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwq\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m128i s;
volatile __m512i res;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtepi16_epi64 (s);
  res = _mm512_mask_cvtepi16_epi64 (res, m, s);
  res = _mm512_maskz_cvtepi16_epi64 (m, s);
}
