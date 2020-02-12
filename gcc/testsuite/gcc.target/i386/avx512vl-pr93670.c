/* PR target/93670 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512dq" } */

#include <x86intrin.h>

__m128i
f1 (__m256i x)
{
  return _mm256_extracti32x4_epi32 (x, 0);
}

__m128i
f2 (__m256i x, __m128i w, __mmask8 m)
{
  return _mm256_mask_extracti32x4_epi32 (w, m, x, 0);
}

__m128i
f3 (__m256i x, __mmask8 m)
{
  return _mm256_maskz_extracti32x4_epi32 (m, x, 0);
}

__m128
f4 (__m256 x)
{
  return _mm256_extractf32x4_ps (x, 0);
}

__m128
f5 (__m256 x, __m128 w, __mmask8 m)
{
  return _mm256_mask_extractf32x4_ps (w, m, x, 0);
}

__m128
f6 (__m256 x, __mmask8 m)
{
  return _mm256_maskz_extractf32x4_ps (m, x, 0);
}

__m128i
f7 (__m256i x)
{
  return _mm256_extracti32x4_epi32 (x, 1);
}

__m128i
f8 (__m256i x, __m128i w, __mmask8 m)
{
  return _mm256_mask_extracti32x4_epi32 (w, m, x, 1);
}

__m128i
f9 (__m256i x, __mmask8 m)
{
  return _mm256_maskz_extracti32x4_epi32 (m, x, 1);
}

__m128
f10 (__m256 x)
{
  return _mm256_extractf32x4_ps (x, 1);
}

__m128
f11 (__m256 x, __m128 w, __mmask8 m)
{
  return _mm256_mask_extractf32x4_ps (w, m, x, 1);
}

__m128
f12 (__m256 x, __mmask8 m)
{
  return _mm256_maskz_extractf32x4_ps (m, x, 1);
}
