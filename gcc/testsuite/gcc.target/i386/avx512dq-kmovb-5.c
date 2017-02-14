/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "kmovb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile unsigned int i;

void
avx512dq_test ()
{
  __mmask8 k = 11;

  asm volatile ("" : "+k" (k));
  i = _cvtmask8_u32 (k);
}
