/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmovzxbd\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

__m128i x;
__m256i res;

void extern
avx2_test (void)
{
  res = _mm256_cvtepu8_epi32 (x);
}
