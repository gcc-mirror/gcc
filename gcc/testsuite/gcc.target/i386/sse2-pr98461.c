/* PR target/98461 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -masm=att" } */
/* { dg-final { scan-assembler-times "\tpmovmskb\t" 6 } } */
/* { dg-final { scan-assembler-times "\txorl\t" 6 } } */
/* { dg-final { scan-assembler-not "\tpcmpeq" } } */
/* { dg-final { scan-assembler-not "\tpxor" } } */
/* { dg-final { scan-assembler-not "\tpandn" } } */

#include <x86intrin.h>

int
f1 (__m128i x)
{
  return _mm_movemask_epi8 (x) ^ 65535;
}

int
f2 (__m128i x)
{
  return _mm_movemask_epi8 (_mm_andnot_si128 (x, _mm_set1_epi8 (255)));
}

int
f3 (__v16qi x)
{
  x ^= (__v16qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return _mm_movemask_epi8 ((__m128i) x);
}

long
f4 (__m128i x)
{
  return (unsigned) (_mm_movemask_epi8 (x) ^ 65535);
}

long
f5 (__m128i x)
{
  return (unsigned) _mm_movemask_epi8 (_mm_andnot_si128 (x, _mm_set1_epi8 (255)));
}

long
f6 (__v16qi x)
{
  x ^= (__v16qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return (unsigned) _mm_movemask_epi8 ((__m128i) x);
}
