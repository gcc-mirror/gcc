/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <xmmintrin.h>

__m64
foo (__m64 i, int w)
{
  return _m_pinsrw (i, w, 2);
}
