/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

int *a;
long long *b;
volatile __m512i zz;
volatile __m512i zz1;

void extern
avx512f_test (void)
{
  zz = _mm512_loadu_epi32 (a);
  _mm512_storeu_epi32 (a, zz);
  zz1 = _mm512_loadu_epi64 (b);
  _mm512_storeu_epi64 (b, zz1);
}
