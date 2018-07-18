/* { dg-do compile } */
/* { dg-options "-O0 -mavx512dq" } */
/* { dg-final { scan-assembler-times "ktestb\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

void
avx512dq_test () {
  volatile __mmask8 k1;
  __mmask8 k2;

  volatile unsigned char r __attribute__((unused));	

  r = _ktestc_mask8_u8(k1, k2);
  r = _ktestz_mask8_u8(k1, k2);
}
