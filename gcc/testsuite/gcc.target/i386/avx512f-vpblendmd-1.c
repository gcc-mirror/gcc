/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "(vpblendmd|vmovdqa32)\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}" } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_blend_epi32 (m, x, x);
}
