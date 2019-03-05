/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vptestnmd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestnmd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  m16 = _mm512_testn_epi32_mask (x, x);
  m16 = _mm512_mask_testn_epi32_mask (3, x, x);
}
