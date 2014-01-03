/* { dg-do compile } */
/* { dg-options "-mavx512cd -O2" } */
/* { dg-final { scan-assembler "vpbroadcastmb2q\[ \\t\]+\[^\n\]*k\[1-7\]\[^\n\]*%zmm\[0-7\]" } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  x = _mm512_broadcastmb_epi64 (m8);
}
