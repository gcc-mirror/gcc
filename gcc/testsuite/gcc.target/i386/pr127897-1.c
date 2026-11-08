/* PR target/127897 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx" } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "movss" } } */
/* { dg-final { scan-assembler-not "movddup" } } */

#include <immintrin.h>

__m128
f (__m128 x)
{
  return _mm_add_ps (_mm_mul_ps (x, _mm_set1_ps (1.5f)), _mm_set1_ps (0.25f));
}

__m128d
g (__m128d x)
{
  return _mm_mul_pd (x, _mm_set1_pd (1.5));
}
