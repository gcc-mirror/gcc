/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "vmovntps\[ \\t\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

float *x;
volatile __m512 y;

void extern
avx512f_test (void)
{
  _mm512_stream_ps (x, y);
}
