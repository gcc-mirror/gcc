/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kxnord\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test ()
{
  __mmask32 k1, k2, k3;
  volatile __m512i x = _mm512_setzero_si512 ();

  __asm__( "kmovd %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovd %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kxnor_mask32 (k1, k2);
  x = _mm512_mask_add_epi16 (x, k3, x, x);
}
