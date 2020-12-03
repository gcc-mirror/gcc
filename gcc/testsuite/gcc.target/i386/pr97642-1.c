/* PR target/97642 */
/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-not { k[0-8] } } } */

#include <immintrin.h>
__m128i
foo1 (__m128i src, void const* P)
{
  return _mm_mask_loadu_epi32 (src, 15, P);
}

__m256i
foo2 (__m256i src, void const* P)
{
  return _mm256_mask_loadu_epi32 (src, 255, P);
}

__m512i
foo3 (__m512i src, void const* P)
{
  return _mm512_mask_loadu_epi32 (src, 65535 , P);
}

__m128i
foo4 (__m128i src, void const* P)
{
  return _mm_mask_loadu_epi32 (src, -1, P);
}

__m256i
foo5 (__m256i src, void const* P)
{
  return _mm256_mask_loadu_epi32 (src, -1, P);
}

__m512i
foo6 (__m512i src, void const* P)
{
  return _mm512_mask_loadu_epi32 (src, -1 , P);
}
