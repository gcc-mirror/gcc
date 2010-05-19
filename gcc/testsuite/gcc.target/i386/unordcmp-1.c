/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler "cmpunordss" } } */
/* { dg-final { scan-assembler "cmpunordps" } } */
/* { dg-final { scan-assembler "cmpunordsd" } } */
/* { dg-final { scan-assembler "cmpunordpd" } } */
/* { dg-final { scan-assembler-not "cmpordss" } } */
/* { dg-final { scan-assembler-not "cmpordps" } } */
/* { dg-final { scan-assembler-not "cmpordsd" } } */
/* { dg-final { scan-assembler-not "cmpordpd" } } */

#include <emmintrin.h>

__m128
f1 (__m128 x, __m128 y)
{
  return _mm_cmpunord_ss (x, y);
}

__m128
f2 (__m128 x, __m128 y)
{
  return _mm_cmpunord_ps (x, y);
}

__m128d
f3 (__m128d x, __m128d y)
{
  return _mm_cmpunord_sd (x, y);
}

__m128d
f4 (__m128d x, __m128d y)
{
  return _mm_cmpunord_pd (x, y);
}
