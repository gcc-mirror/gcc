/* PR target/95905 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx" } */
/* { dg-final { scan-assembler "\tpmovzxbw\t" } } */
/* { dg-final { scan-assembler "\tpmovzxwd\t" } } */
/* { dg-final { scan-assembler "\tpmovzxdq\t" } } */

#include <x86intrin.h>

__m128i
f1 (__m128i a)
{
  return _mm_unpacklo_epi8 (a, _mm_setzero_si128 ());
}

__m128i
f2 (__m128i a)
{
  return _mm_unpacklo_epi16 (a, _mm_setzero_si128 ());
}

__m128i
f3 (__m128i a)
{
  return _mm_unpacklo_epi32 (a, _mm_setzero_si128 ());
}
