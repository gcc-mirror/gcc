/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-final { scan-assembler-times "pminsd" 2 } } */
/* { dg-final { scan-assembler-times "pmaxsd" 2 } } */

#include <smmintrin.h>

__m128i min32(__m128i value, __m128i input)
{
  return _mm_blendv_epi8(input, value, _mm_cmplt_epi32(value, input));
}

__m128i max32(__m128i value, __m128i input)
{
  return _mm_blendv_epi8(input, value, _mm_cmpgt_epi32(value, input));
}

__m128i min32_1(__m128i value, __m128i input)
{
  return _mm_blendv_epi8(input, value, _mm_cmpgt_epi32(input, value));
}

__m128i max32_1(__m128i value, __m128i input)
{
  return _mm_blendv_epi8(input, value, _mm_cmplt_epi32(input, value));
}

