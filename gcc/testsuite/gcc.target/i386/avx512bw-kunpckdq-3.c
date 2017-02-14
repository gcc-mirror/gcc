/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kunpckdq\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test () {
  volatile __mmask64 k3;
  __mmask32 k1, k2;

  __asm__( "kmovd %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovd %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _kunpackd_mask64 (k1, k2);
}
