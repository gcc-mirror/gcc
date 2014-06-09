/* { dg-do compile } */
/* { dg-options "-O0 -mavx512f" } */
/* { dg-final { scan-assembler-times "kortestw\[ \\t\]+\[^\n\]*%k\[0-7\]" 4 } } */

#include <immintrin.h>

void
avx512f_test () {
  volatile __mmask16 k1 = 0;
  __mmask16 k2 = 0;
  volatile __mmask8 k3 = 0;
  __mmask8 k4 = 0;

  volatile short r __attribute__((unused));

  /* Check that appropriate insn sequence is generated at -O0.  */
  r = _mm512_kortestc (k1, k2);
  r = _mm512_kortestz (k1, k2);

  r = _mm512_kortestc (k3, k4);
  r = _mm512_kortestz (k3, k4);
}
