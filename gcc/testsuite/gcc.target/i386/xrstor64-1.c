/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mxsave -O2" } */
/* { dg-final { scan-assembler "xrstor64\[ \\t\]" } } */

#include <x86intrin.h>

void extern
xsave_test (void)
{
  char xsave_region [512] __attribute__((aligned(64)));
  _xrstor64 (xsave_region, ((long long) 0xA0000000F));
}
