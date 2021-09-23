/* PR target/99908 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -masm=att" } */
/* { dg-final { scan-assembler-times "\tvpblendvb\t" 2 } } */
/* { dg-final { scan-assembler-not "\tvpcmpeq" } } */
/* { dg-final { scan-assembler-not "\tvpandn" } } */

#include <x86intrin.h>

__m256i
f1 (__m256i a, __m256i b, __m256i mask)
{
  return _mm256_blendv_epi8(a, b, 
    _mm256_andnot_si256(mask, _mm256_set1_epi8(255)));
}

__m256i
f2 (__v32qi x, __v32qi a, __v32qi b)
{
  x ^= (__v32qi) { -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1,
		   -1, -1, -1, -1, -1, -1, -1, -1 };
  return _mm256_blendv_epi8 ((__m256i) a, (__m256i) b, (__m256i) x);
}
