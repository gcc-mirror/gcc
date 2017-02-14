/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "kmovw\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __mmask16 k1;

void
avx512f_test ()
{
  __mmask16 k0 = 11; 
  __mmask16 k = _load_mask16 (&k0);

  asm volatile ("" : "+k" (k));
  k1 = k;
}
