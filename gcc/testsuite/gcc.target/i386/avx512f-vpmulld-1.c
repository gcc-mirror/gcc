/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}{z}" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 mx;

void extern
avx512f_test (void)
{
  x = _mm512_mullo_epi32 (x, x);
  x = _mm512_mask_mullo_epi32 (x, mx, x, x);
  x = _mm512_maskz_mullo_epi32 (mx, x, x);
}
