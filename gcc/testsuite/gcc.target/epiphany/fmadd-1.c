/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmadd\[ \ta-zA-Z0-9\]*," 2 } } */

#include <epiphany_intrinsics.h>

float
f1 (float a, float b, float c)
{
  return __builtin_epiphany_fmadd (a, b, c);
}

float
f2 (float a, float b, float c)
{
  return a + b * c;
}
