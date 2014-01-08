/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vunpcklps\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vunpcklps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vunpcklps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512 x, y, z;

void extern
avx512f_test (void)
{
  x = _mm512_unpacklo_ps (y, z);
  x = _mm512_mask_unpacklo_ps (x, 2, y, z);
  x = _mm512_maskz_unpacklo_ps (2, y, z);
}
