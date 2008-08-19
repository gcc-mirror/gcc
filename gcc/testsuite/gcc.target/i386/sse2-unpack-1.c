/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

__m128i
foo1 (__m128i s1, __m128i s2)
{
  return _mm_unpackhi_epi64 (s1, s2); 
}

__m128i
foo2 (__m128i s1, __m128i s2)
{
  return _mm_unpacklo_epi64 (s1, s2); 
}

/* { dg-final { scan-assembler "punpcklqdq" } } */
/* { dg-final { scan-assembler "punpckhqdq" } } */
