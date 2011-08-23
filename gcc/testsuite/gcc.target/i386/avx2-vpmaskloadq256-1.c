/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmaskmovq\[ \\t\]+\[^\n\]" } } */

#include <immintrin.h>

volatile __m256i x;
long long int *y;

void extern
avx2_test (void)
{
  x = _mm256_maskload_epi64 (y, x);
}
