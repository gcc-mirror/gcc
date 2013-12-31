/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\n\]*%ymm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\n\]*%ymm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\n\]*%ymm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m256i s;
volatile __m512i res;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtepi16_epi32 (s);
  res = _mm512_mask_cvtepi16_epi32 (res, m, s);
  res = _mm512_maskz_cvtepi16_epi32 (m, s);
}
