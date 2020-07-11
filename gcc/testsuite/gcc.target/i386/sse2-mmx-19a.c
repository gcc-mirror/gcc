/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx -mtune=intel" } */
/* { dg-final { scan-assembler-times "pshuflw" 1 } } */
/* { dg-final { scan-assembler-times "movd" 1 } } */
/* { dg-final { scan-assembler-not "movl" } } */

#include <mmintrin.h>

__m64
foo (short i)
{
  __v4hi x = { i, i, i, i };
  return (__m64) x;
}
