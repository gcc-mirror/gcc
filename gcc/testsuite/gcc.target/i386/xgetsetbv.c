/* { dg-do compile } */
/* { dg-options "-O2 -mxsave" } */
/* { dg-final { scan-assembler "xgetbv" } } */
/* { dg-final { scan-assembler "xsetbv" } } */

#include <x86intrin.h>

unsigned int
xgetsetbv (void)
{
 _xsetbv (0, 0);
  return _xgetbv (0);
}
