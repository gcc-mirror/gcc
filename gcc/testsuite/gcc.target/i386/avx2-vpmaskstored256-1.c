/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmaskmovd\[ \\t\]+\[^\n\]" } } */

#include <immintrin.h>

volatile __m256i x;
int *y;

void extern
avx2_test (void)
{
  _mm256_maskstore_epi32 (y, x, x);
}
