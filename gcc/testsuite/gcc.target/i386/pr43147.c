/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler "movaps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

#include <x86intrin.h>

__m128
foo (void)
{
  __m128 m = _mm_set_ps(1.0f, 2.0f, 3.0f, 4.0f);
  m = _mm_shuffle_ps(m, m, 0xC9);
  m = _mm_shuffle_ps(m, m, 0x2D);
  return m;
}
