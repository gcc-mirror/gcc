#include <immintrin.h>
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-times "pabs" 3 } } */

__m64
absb_64 (__m64 a)
{
  return _mm_abs_pi8(a);
}

__m64
absw_64 (__m64 a)
{
  return _mm_abs_pi16(a);
}

__m64
absd_64 (__m64 a)
{
  return _mm_abs_pi32(a);
}
