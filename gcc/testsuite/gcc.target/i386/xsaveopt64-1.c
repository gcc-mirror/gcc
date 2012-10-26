/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mxsaveopt -O2" } */
/* { dg-final { scan-assembler "xsaveopt64\[ \\t\]" } } */

#include <x86intrin.h>

void extern
xsave_test (void)
{
  char xsaveopt_region [512] __attribute__((aligned(64)));
  _xsaveopt64 (xsaveopt_region, ((long long) 0xA0000000F));
}
