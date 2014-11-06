/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler "vmovntdqa\[ \\t\]+\[^\n\]*%zmm\[0-9\]"} } */

#include <immintrin.h>

volatile __m512i x;
__m512i *y;

void extern
avx512vl_test (void)
{
  x = _mm512_stream_load_si512 (y);
}
