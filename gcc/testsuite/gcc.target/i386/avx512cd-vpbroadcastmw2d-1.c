/* { dg-do compile } */
/* { dg-options "-mavx512cd -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastmw2d\[ \\t\]+\[^\{\n\]*k\[1-7\]\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  x = _mm512_broadcastmw_epi32 (m16);
}
