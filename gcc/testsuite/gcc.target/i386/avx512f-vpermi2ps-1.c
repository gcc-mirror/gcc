/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpermi2ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m512i y;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask2_permutex2var_ps (x, y, m, x);
}
