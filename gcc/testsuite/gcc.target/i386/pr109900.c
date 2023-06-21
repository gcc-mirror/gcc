#include <immintrin.h>
/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2 -mavx512vl -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "builtin_ia32_pabs" "optimized" { target { ! ia32 } } } } */


__m64
absb_64 ()
{
  return _mm_abs_pi8(_mm_set1_pi8 (-1));
}

__m128i
absb_128 ()
{
  return _mm_abs_epi8(_mm_set1_epi8 (-1));
}

__m256i
absb_256 ()
{
  return _mm256_abs_epi8(_mm256_set1_epi8 (-1));
}

__m512i
absb_512 ()
{
  return _mm512_abs_epi8(_mm512_set1_epi8 (-1));
}

__m64
absw_64 ()
{
  return _mm_abs_pi16(_mm_set1_pi16 (-1));
}

__m128i
absw_128 ()
{
  return _mm_abs_epi16(_mm_set1_epi16 (-1));
}

__m256i
absw_256 ()
{
  return _mm256_abs_epi16(_mm256_set1_epi16 (-1));
}

__m512i
absw_512 ()
{
  return _mm512_abs_epi16(_mm512_set1_epi16 (-1));
}

__m64
absd_64 ()
{
  return _mm_abs_pi32(_mm_set1_pi32 (-1));
}

__m128i
absd_128 ()
{
  return _mm_abs_epi32(_mm_set1_epi32 (-1));
}

__m256i
absd_256 ()
{
  return _mm256_abs_epi32(_mm256_set1_epi32 (-1));
}

__m512i
absd_512 ()
{
  return _mm512_abs_epi32(_mm512_set1_epi32 (-1));
}

__m128i
absq_128 ()
{
  return _mm_abs_epi64(_mm_set1_epi64x (-1));
}

__m256i
absq_256 ()
{
  return _mm256_abs_epi64(_mm256_set1_epi64x (-1));
}

__m512i
absq_512 ()
{
  return _mm512_abs_epi64(_mm512_set1_epi64 (-1));
}
