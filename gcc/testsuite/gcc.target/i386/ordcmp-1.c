/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler "cmpordss" } } */
/* { dg-final { scan-assembler "cmpordps" } } */
/* { dg-final { scan-assembler "cmpordsd" } } */
/* { dg-final { scan-assembler "cmpordpd" } } */
/* { dg-final { scan-assembler-not "cmpunordss" } } */
/* { dg-final { scan-assembler-not "cmpunordps" } } */
/* { dg-final { scan-assembler-not "cmpunordsd" } } */
/* { dg-final { scan-assembler-not "cmpunordpd" } } */

#include <emmintrin.h>

__m128
f1 (__m128 x, __m128 y)
{
  return _mm_cmpord_ss (x, y);
}

__m128
f2 (__m128 x, __m128 y)
{
  return _mm_cmpord_ps (x, y);
}

__m128d
f3 (__m128d x, __m128d y)
{
  return _mm_cmpord_sd (x, y);
}

__m128d
f4 (__m128d x, __m128d y)
{
  return _mm_cmpord_pd (x, y);
}
