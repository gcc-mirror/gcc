/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <mmintrin.h>

float
foo (__m64 x)
{
  return ((__v2sf) x)[0];
}
