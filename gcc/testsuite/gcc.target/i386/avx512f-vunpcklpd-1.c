/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512d x, y, z;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_unpacklo_pd (y, z);
  x = _mm512_mask_unpacklo_pd (x, m, y, z);
  x = _mm512_maskz_unpacklo_pd (m, y, z);
}
