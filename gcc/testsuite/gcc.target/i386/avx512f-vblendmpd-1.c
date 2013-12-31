/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "(vblendmpd|vmovapd)\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}" } } */

#include <immintrin.h>

volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_blend_pd (m, x, x);
}
