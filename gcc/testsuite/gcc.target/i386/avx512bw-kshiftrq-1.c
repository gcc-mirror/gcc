/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kshiftrq\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test ()
{
  __mmask64 k1, k2;
  unsigned int i = 5;
  volatile __m512i x = _mm512_setzero_si512 ();

  __asm__( "kmovq %1, %0" : "=k" (k1) : "r" (1ULL) );

  k2 = _kshiftri_mask64 (k1, i);
  x = _mm512_mask_add_epi8 (x, k2, x, x);
}
