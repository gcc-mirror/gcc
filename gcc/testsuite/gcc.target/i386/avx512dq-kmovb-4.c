/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "kmovb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __mmask8 k1 = 11;

void
avx512bw_test ()
{
  __mmask8 k0, k;
 
  _store_mask8 (&k, k1);

  asm volatile ("" : "+k" (k));
  k0 = k;
}
