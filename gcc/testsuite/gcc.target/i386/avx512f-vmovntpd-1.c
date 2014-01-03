/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "vmovntpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

double *x;
volatile __m512d y;

void extern
avx512f_test (void)
{
  _mm512_stream_pd (x, y);
}
