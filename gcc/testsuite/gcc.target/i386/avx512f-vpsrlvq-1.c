/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vpsrlvq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vpsrlvq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlvq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m512i y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_srlv_epi64 (x, y);
  x = _mm512_mask_srlv_epi64 (x, m, x, y);
  x = _mm512_maskz_srlv_epi64 (m, x, y);
}
