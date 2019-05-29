/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler "cvtdq2ps" } } */
/* { dg-final { scan-assembler-not "cvtpi2ps" } } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <xmmintrin.h>

__m128
foo (__m128 i1, __m64 i2)
{
  return _mm_cvtpi32_ps (i1, i2);
}
