/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "kmovb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __mmask8 k1;

void
avx512dq_test ()
{
  __mmask8 k = _cvtu32_mask8 (11);

  asm volatile ("" : "+k" (k));
  k1 = k;
}
