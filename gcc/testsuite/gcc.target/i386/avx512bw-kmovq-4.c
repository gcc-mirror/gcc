/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "kmovq\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile unsigned long long i;

void
avx512bw_test ()
{
  __mmask64 k = 11;
  
  asm volatile ("" : "+k" (k));
  i = _cvtmask64_u64 (k);
}
