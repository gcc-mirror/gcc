/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -m3dnowa" } */

#include <mm3dnow.h>

__m64
foo (__m64 x, __m64 y)
{
  return _m_pfnacc (x, y);
}
