/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kandq\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test ()
{
  __mmask64 k1, k2, k3;
  volatile __m512i x = _mm512_setzero_epi32();

  __asm__( "kmovq %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovq %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kand_mask64 (k1, k2);
  x = _mm512_mask_add_epi8 (x, k3, x, x);
}
