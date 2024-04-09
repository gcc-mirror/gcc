/* PR target/114576 */
/* { dg-do compile } */
/* { dg-options "-O2 -maes -mno-avx" } */
/* { dg-final { scan-assembler-times "\taesenc\t" 2 } } */
/* { dg-final { scan-assembler-times "\taesdec\t" 2 } } */
/* { dg-final { scan-assembler-times "\taesenclast\t" 2 } } */
/* { dg-final { scan-assembler-times "\taesdeclast\t" 2 } } */
/* { dg-final { scan-assembler-not "\tvaesenc" } } */
/* { dg-final { scan-assembler-not "\tvaesdec" } } */

#include <immintrin.h>

__m128i
f1 (__m128i x, __m128i y)
{
  return _mm_aesenc_si128 (x, y);
}

__m128i
f2 (__m128i x, __m128i y)
{
  __m128i z = _mm_aesenc_si128 (x, y);
  return z + x + y;
}

__m128i
f3 (__m128i x, __m128i y)
{
  return _mm_aesdec_si128 (x, y);
}

__m128i
f4 (__m128i x, __m128i y)
{
  __m128i z = _mm_aesdec_si128 (x, y);
  return z + x + y;
}

__m128i
f5 (__m128i x, __m128i y)
{
  return _mm_aesenclast_si128 (x, y);
}

__m128i
f6 (__m128i x, __m128i y)
{
  __m128i z = _mm_aesenclast_si128 (x, y);
  return z + x + y;
}

__m128i
f7 (__m128i x, __m128i y)
{
  return _mm_aesdeclast_si128 (x, y);
}

__m128i
f8 (__m128i x, __m128i y)
{
  __m128i z = _mm_aesdeclast_si128 (x, y);
  return z + x + y;
}
