/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "kmovw\[ \\t\]+\[^\n\]*%k\[0-7\]" } } */

#include <immintrin.h>
volatile __mmask16 k1;

void
avx512f_test ()
{
  k1 = _mm512_kmov (11);
}
