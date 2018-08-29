/* { dg-do compile } */
/* { dg-options "-O2 -mxsave" } */
/* { dg-final { scan-assembler-times "xgetbv" 3 } } */
/* { dg-final { scan-assembler-times "xsetbv" 3 } } */

#include <x86intrin.h>

unsigned long long
foo (unsigned x, unsigned y)
{
  _xsetbv (x, y);
  return _xgetbv (x);
}

unsigned long long
bar (unsigned x, unsigned long long y)
{
  _xsetbv (x, y);
  return _xgetbv (x);
}

unsigned long long
baz (void)
{
  _xsetbv (0, 0);
  return _xgetbv (0);
}
