/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmaskmovq\[ \\t\]+\[^\n\]" } } */

#include <immintrin.h>

volatile __m256i x;
long long int *y;

void extern
avx2_test (void)
{
  _mm256_maskstore_epi64 (y, x, x);
}
