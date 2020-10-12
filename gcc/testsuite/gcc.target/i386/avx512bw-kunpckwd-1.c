/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kunpckwd\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512bw_test () {
  volatile __mmask32 k1, k2, k3;
  volatile __m256i x;

  __asm__( "kmovd %1, %0" : "=k" (k1) : "r" (1) );
  __asm__( "kmovd %1, %0" : "=k" (k2) : "r" (2) );

  k3 = _mm512_kunpackw (k1, k2);
  //x = _mm256_mask_avg_epu8 (x, k3, x, x);
}
