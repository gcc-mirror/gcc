/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovntdq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

__m512i *x;
volatile __m512i y;

void extern
avx512f_test (void)
{
  _mm512_stream_si512 (x, y);
}
