/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <immintrin.h>

__m128i
foo (__m256i x, __m128i y)
{
  __m256i r = _mm256_insertf128_si256(x, y, 1);
  __m128i a = _mm256_extractf128_si256(r, 1);
  return a;
}

/* { dg-final { scan-assembler-not "vinsertf" } } */
/* { dg-final { scan-assembler-not "vextractf" } } */
