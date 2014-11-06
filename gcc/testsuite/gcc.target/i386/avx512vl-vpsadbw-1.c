/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -O2" } */
/* { dg-final { scan-assembler "vpsadbw\[ \\t\]+\[^\n\]*%zmm\[0-9\]"} } */

#include <immintrin.h>

volatile __m512i x;

void extern
avx512vl_test (void)
{
  x = _mm512_sad_epu8 (x, x);
}
