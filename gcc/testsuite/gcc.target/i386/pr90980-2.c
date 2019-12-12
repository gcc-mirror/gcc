/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu\[0-9\]*\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

int *a;
long long *b;
volatile __m256i yy;
volatile __m256i yy1;

void extern
avx512vl_test (void)
{
  _mm256_storeu_epi32 (a, yy);
  _mm256_storeu_epi64 (b, yy1);
}
