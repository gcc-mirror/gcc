/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt " } */
/* { dg-final { scan-assembler "lzcntw\[^\\n]*(%|)\[ad\]\[xi\]" } } */

#include <x86intrin.h>

unsigned int
func_lzcnt16 (unsigned int X)
{
  return __lzcnt16(X);
}
