/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "kmovw\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile unsigned int i;

void
avx512f_test ()
{
  __mmask16 k = 11;
  
  asm volatile ("" : "+k" (k));
  i = _cvtmask16_u32 (k);
}
