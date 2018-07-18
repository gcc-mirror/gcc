/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kmovd\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile unsigned int i;

void
avx512bw_test ()
{
  __mmask32 k = 11;
  
  asm volatile ("" : "+k" (k));
  i = _cvtmask32_u32 (k);
}
