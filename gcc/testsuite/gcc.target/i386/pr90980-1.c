/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu\[2346\]*\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

int *a;
long long *b;
volatile __m128i xx;
volatile __m128i xx1;

void extern
avx512vl_test (void)
{
  _mm_storeu_epi32 (a, xx);
  _mm_storeu_epi64 (b, xx1);
}
