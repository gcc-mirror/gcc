/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "kaddb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

void
avx512dq_test ()
{
  __mmask8 k = _kadd_mask8 (11, 12);
  asm volatile ("" : "+k" (k));
}
