/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-not {pblendv} } } */
/* { dg-final { scan-assembler-not {blendvp} } } */

#include <x86intrin.h>
__m128
foo (__m128 a, __m128 b)
{
  return _mm_blendv_ps (a, b, _mm_setzero_ps ());
}

__m256
foo2 (__m256 a, __m256 b)
{
  return _mm256_blendv_ps (a, b, _mm256_set1_ps (-1.0));
}

__m128d
foo3 (__m128d a, __m128d b, __m128d c)
{
  return _mm_blendv_pd (a, b, _mm_set1_pd (1.0));
}

__m256d
foo4 (__m256d a, __m256d b, __m256d c)
{
  return _mm256_blendv_pd (a, b, _mm256_set1_pd (-134.3));
}

__m128i
foo5 (__m128i a, __m128i b, __m128i c)
{
  return _mm_blendv_epi8 (a, b, _mm_set1_epi8 (3));
}

__m256i
foo6 (__m256i a, __m256i b, __m256i c)
{
  return _mm256_blendv_epi8 (a, b, _mm256_set1_epi8 (-22));
}
