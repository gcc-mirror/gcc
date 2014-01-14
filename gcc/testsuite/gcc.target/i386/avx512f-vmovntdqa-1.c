/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "vmovntdqa\[ \\t\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

__m512i *x;
volatile __m512i y;

void extern
avx512f_test (void)
{
  y = _mm512_stream_load_si512 (x);
}
