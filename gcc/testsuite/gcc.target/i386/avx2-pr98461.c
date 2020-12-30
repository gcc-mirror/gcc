/* PR target/98461 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -masm=att" } */
/* { dg-final { scan-assembler-times "\tvpmovmskb\t" 6 } } */
/* { dg-final { scan-assembler-times "\tnotl\t" 6 } } */
/* { dg-final { scan-assembler-not "\tvpcmpeq" } } */
/* { dg-final { scan-assembler-not "\tvpxor" } } */
/* { dg-final { scan-assembler-not "\tvpandn" } } */

#include <x86intrin.h>

int
f1 (__m256i x)
{
  return ~_mm256_movemask_epi8 (x);
}

int
f2 (__m256i x)
{
  return _mm256_movemask_epi8 (_mm256_andnot_si256 (x, _mm256_set1_epi8 (255)));
}

int
f3 (__v32qi x)
{
  x ^= (__v32qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return _mm256_movemask_epi8 ((__m256i) x);
}

long
f4 (__m256i x)
{
  return (unsigned) ~_mm256_movemask_epi8 (x);
}

long
f5 (__m256i x)
{
  return (unsigned) _mm256_movemask_epi8 (_mm256_andnot_si256 (x, _mm256_set1_epi8 (255)));
}

long
f6 (__v32qi x)
{
  x ^= (__v32qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return (unsigned) _mm256_movemask_epi8 ((__m256i) x);
}
