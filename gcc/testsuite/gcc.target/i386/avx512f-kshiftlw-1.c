/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "kshiftlw\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512f_test ()
{
  __mmask16 k1, k2;
  unsigned int i = 5;
  volatile __m512 x = _mm512_setzero_ps();

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (1) );

  k2 = _kshiftli_mask16 (k1, i);
  x = _mm512_mask_add_ps (x, k2, x, x);
}
