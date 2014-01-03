/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[\\n\]" 1 } } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i x, y, z;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_unpackhi_epi64 (y, z);
  x = _mm512_mask_unpackhi_epi64 (x, m, y, z);
  x = _mm512_maskz_unpackhi_epi64 (m, y, z);
}
