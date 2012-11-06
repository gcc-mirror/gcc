/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mfxsr -O2" } */
/* { dg-final { scan-assembler "fxsave64\[ \\t\]" } } */

#include <x86intrin.h>

void extern
fxsave_test (void)
{
  char fxsave_region [512] __attribute__((aligned(16)));
  _fxsave64 (fxsave_region);
}
