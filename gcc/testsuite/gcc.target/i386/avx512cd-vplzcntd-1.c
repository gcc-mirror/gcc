/* { dg-do compile } */
/* { dg-options "-mavx512cd -O2" } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1} } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512i s;
volatile __m512i res;

void extern
avx512f_test (void)
{
  res = _mm512_lzcnt_epi32 (s);
  res = _mm512_mask_lzcnt_epi32 (res, 2, s);
  res = _mm512_maskz_lzcnt_epi32 (2, s);
}
