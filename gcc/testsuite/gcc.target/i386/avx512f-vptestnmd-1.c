/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vptestnmd\[ \\t\]+\[^\n\]*%zmm\[0-7\]\[^\n^k\]*k\[1-7\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vptestnmd\[ \\t\]+\[^\n\]*%zmm\[0-7\]\[^\n^k\]*k\[1-7\]\{" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  m16 = _mm512_testn_epi32_mask (x, x);
  m16 = _mm512_mask_testn_epi32_mask (3, x, x);
}
