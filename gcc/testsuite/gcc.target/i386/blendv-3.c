/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times {vp?blendv(?:b|p[sd])[ \t]*} 6 } } */
/* { dg-final { scan-assembler-not {vpcmp} } } */

#include <immintrin.h>

__m256i
foo (__m256i a, __m256i b, __m256i c)
{
  return _mm256_blendv_epi8 (a, b, ~c < 0);
}

__m256d
foo1 (__m256d a, __m256d b, __m256i c)
{
  __m256i d = ~c < 0;
  return _mm256_blendv_pd (a, b, (__m256d)d);
}

__m256
foo2 (__m256 a, __m256 b, __m256i c)
{
  __m256i d = ~c < 0;
  return _mm256_blendv_ps (a, b, (__m256)d);
}

__m128i
foo4 (__m128i a, __m128i b, __m128i c)
{
  return _mm_blendv_epi8 (a, b, ~c < 0);
}

__m128d
foo5 (__m128d a, __m128d b, __m128i c)
{
  __m128i d = ~c < 0;
  return _mm_blendv_pd (a, b, (__m128d)d);
}

__m128
foo6 (__m128 a, __m128 b, __m128i c)
{
  __m128i d = ~c < 0;
  return _mm_blendv_ps (a, b, (__m128)d);
}
