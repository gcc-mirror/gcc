/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "kxorw\[ \\t\]+\[^\n\]*%k\[1-7\]" 1 } } */

#include <immintrin.h>

void
avx512f_test ()
{
  __mmask16 k1, k2, k3;
  volatile __m512 x;

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _mm512_kxor (k1, k2);
  x = _mm512_mask_add_ps (x, k3, x, x);
}
