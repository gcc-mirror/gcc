/* { dg-do compile } */
/* { dg-options "-mavx2 -mno-avx512f -O2" } */
/* { dg-final { scan-assembler "vpandn\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;

void extern
avx2_test (void)
{
  x = _mm256_andnot_si256 (x, x);
}
