/* { dg-do compile } */
/* { dg-options "-mfxsr -O2" } */
/* { dg-final { scan-assembler "fxrstor\[ \\t\]" } } */

#include <x86intrin.h>

void extern
fxsave_test (void)
{
  char fxsave_region [512] __attribute__((aligned(16)));
  _fxrstor (fxsave_region);
}
