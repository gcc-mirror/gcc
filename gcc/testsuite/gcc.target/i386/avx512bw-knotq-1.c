/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "knotq\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test ()
{
  __mmask64 k1, k2;
  volatile __m512i x = _mm512_setzero_si512 ();

  __asm__( "kmovq %1, %0" : "=k" (k1) : "r" (45) );

  k2 = _knot_mask64 (k1);
  x = _mm512_mask_add_epi8 (x, k1, x, x);
  x = _mm512_mask_add_epi8 (x, k2, x, x);
}
