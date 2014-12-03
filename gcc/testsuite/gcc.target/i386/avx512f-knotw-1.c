/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "knotw\[ \\t\]+\[^\{\n\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512f_test ()
{
  __mmask16 k1, k2;
  volatile __m512 x = _mm512_setzero_ps(); 

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (45) );

  k2 = _mm512_knot (k1);

  x = _mm512_mask_add_ps (x, k1, x, x);
  x = _mm512_mask_add_ps (x, k2, x, x);
}
