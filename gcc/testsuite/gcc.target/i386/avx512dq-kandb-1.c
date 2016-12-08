/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "kandb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512dq_test ()
{
  __mmask8 k1, k2, k3;
  volatile __m512i x = _mm512_setzero_epi32();

  __asm__( "kmovb %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovb %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kand_mask8 (k1, k2);
  x = _mm512_mask_add_epi64 (x, k3, x, x);
}
