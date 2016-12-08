/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "korb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512dq_test ()
{
  __mmask8 k1, k2, k3;
  volatile __m512d x = _mm512_setzero_pd();

  __asm__( "kmovb %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovb %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kor_mask8 (k1, k2);
  x = _mm512_mask_add_pd (x, k3, x, x);
}
