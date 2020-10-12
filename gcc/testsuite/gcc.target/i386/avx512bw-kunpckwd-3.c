/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kunpckwd\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test () {
  volatile __mmask32 k3;
  __mmask16 k1, k2;

  __asm__( "kmovw %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovw %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kunpackw_mask32 (k1, k2);
}
