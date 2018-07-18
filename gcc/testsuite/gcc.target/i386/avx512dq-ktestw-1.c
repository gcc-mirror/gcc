/* { dg-do compile } */
/* { dg-options "-O0 -mavx512dq" } */
/* { dg-final { scan-assembler-times "ktestw\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

void
avx512dq_test () {
  volatile __mmask16 k1;
  __mmask16 k2;

  volatile unsigned char r __attribute__((unused));	

  r = _ktestc_mask16_u8(k1, k2);
  r = _ktestz_mask16_u8(k1, k2);
}
