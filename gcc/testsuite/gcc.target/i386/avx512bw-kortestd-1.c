/* { dg-do compile } */
/* { dg-options "-O0 -mavx512bw" } */
/* { dg-final { scan-assembler-times "kortestd\[ \\t\]+\[^\{\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

void
avx512bw_test () {
  volatile __mmask32 k1;
  __mmask32 k2;

  volatile unsigned char r __attribute__((unused));	

  r = _kortestc_mask32_u8(k1, k2);
  r = _kortestz_mask32_u8(k1, k2);
}
