/* { dg-do compile } */
/* { dg-options "-O0 -mlzcnt " } */
/* { dg-final { scan-assembler "lzcntl\[^\\n]*(%|)eax" } } */

#include <x86intrin.h>

unsigned int
func_lzcnt32 (unsigned int X)
{
  return __lzcnt32(X);
}
