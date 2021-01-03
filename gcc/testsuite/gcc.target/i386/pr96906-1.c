/* PR target/96906 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler-times "\tvpminub\[^\n\r]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpminuw\[^\n\r]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpminub\[^\n\r]*ymm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpminuw\[^\n\r]*ymm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpcmpeqb\[^\n\r]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpcmpeqw\[^\n\r]*xmm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpcmpeqb\[^\n\r]*ymm" 2 } } */
/* { dg-final { scan-assembler-times "\tvpcmpeqw\[^\n\r]*ymm" 2 } } */
/* { dg-final { scan-assembler-not "\tvpsubus\[bw]" } } */

#include <x86intrin.h>

__m128i
f1 (__m128i x, __m128i y)
{
  return _mm_cmpeq_epi16 (_mm_subs_epu16 (x, y), _mm_setzero_si128 ());
}

__m128i
f2 (__m128i x, __m128i y)
{
  return _mm_cmpeq_epi16 (_mm_min_epu16 (x, y), x);
}

__m128i
f3 (__m128i x, __m128i y)
{
  return _mm_cmpeq_epi8 (_mm_subs_epu8 (x, y), _mm_setzero_si128 ());
}

__m128i
f4 (__m128i x, __m128i y)
{
  return _mm_cmpeq_epi8 (_mm_min_epu8 (x, y), x);
}

__m256i
f5 (__m256i x, __m256i y)
{
  return _mm256_cmpeq_epi16 (_mm256_subs_epu16 (x, y), _mm256_setzero_si256 ());
}

__m256i
f6 (__m256i x, __m256i y)
{
  return _mm256_cmpeq_epi16 (_mm256_min_epu16 (x, y), x);
}

__m256i
f7 (__m256i x, __m256i y)
{
  return _mm256_cmpeq_epi8 (_mm256_subs_epu8 (x, y), _mm256_setzero_si256 ());
}

__m256i
f8 (__m256i x, __m256i y)
{
  return _mm256_cmpeq_epi8 (_mm256_min_epu8 (x, y), x);
}
