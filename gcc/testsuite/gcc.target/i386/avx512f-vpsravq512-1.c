/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "vpsravq\[ \\t\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

volatile __m512i x;

void extern
avx512f_test (void)
{
  x = _mm512_srav_epi64 (x, x);
}