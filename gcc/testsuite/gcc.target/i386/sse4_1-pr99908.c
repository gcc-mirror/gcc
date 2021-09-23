/* PR target/99908 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx -masm=att" } */
/* { dg-final { scan-assembler-times "\tpblendvb\t" 2 } } */
/* { dg-final { scan-assembler-not "\tpcmpeq" } } */
/* { dg-final { scan-assembler-not "\tpandn" } } */

#include <x86intrin.h>

__m128i
f1 (__m128i a, __m128i b, __m128i mask)
{
  return _mm_blendv_epi8(a, b, 
    _mm_andnot_si128(mask, _mm_set1_epi8(255)));
}

__m128i
f2 (__v16qi x, __v16qi a, __v16qi b)
{
  x ^= (__v16qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return _mm_blendv_epi8 ((__m128i) a, (__m128i) b, (__m128i) x);
}
