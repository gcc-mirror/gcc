/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpermpd\[ \\t\]+\[^\n\]*" } } */

#include <immintrin.h>

__m256d x;

void extern
avx2_test (void)
{
  x = _mm256_permute4x64_pd (x, 13);
}
