/* { dg-do compile } */
/* { dg-options "-mavx -O2" } */
/* { dg-final { scan-assembler "xrstor\[ \\t\]" } } */

#include <x86intrin.h>

void extern
xsave_test (void)
{
  char xsave_region [512] __attribute__((aligned(64)));
  _xrstor (xsave_region, ((long long) 0xA0000000F));
}
