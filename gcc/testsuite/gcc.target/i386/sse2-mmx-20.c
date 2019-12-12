/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler-times "movd" 1 } } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <mmintrin.h>

int
foo (__m64 x)
{
  return ((__v2si) x)[0];
}
