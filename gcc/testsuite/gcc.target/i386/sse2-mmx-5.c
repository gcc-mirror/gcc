/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <xmmintrin.h>

int
foo (__m64 i)
{
  return _m_pextrw (i, 2);
}
