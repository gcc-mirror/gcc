/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kmovd\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __mmask32 k1;

void
avx512bw_test ()
{
  __mmask32 k = _cvtu32_mask32 (11);

  asm volatile ("" : "+k" (k));
  k1 = k;
}
