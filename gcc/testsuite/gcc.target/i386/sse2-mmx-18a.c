/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx -mno-avx512vl" } */
/* { dg-final { scan-assembler-times "pshufd" 1 } } */
/* { dg-final { scan-assembler-times "movd" 1 } } */
/* { dg-final { scan-assembler-not "movl" } } */

#include <mmintrin.h>

__m64
foo (int i)
{
  __v2si x = { i, i };
  return (__m64) x;
}
