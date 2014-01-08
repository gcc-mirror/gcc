/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i x, y, z;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_ternarylogic_epi32 (x, y, z, 0xF0);
  x = _mm512_mask_ternarylogic_epi32 (x, m, y, z, 0xF0);
  x = _mm512_maskz_ternarylogic_epi32 (m, x, y, z, 0xF0);
}
