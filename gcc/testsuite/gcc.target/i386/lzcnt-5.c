/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-final { scan-assembler "lzcntq\[^\\n]*(%|)rax" } } */

#include <x86intrin.h>

unsigned int
func_lzcnt64 (unsigned long long X)
{
  return __lzcnt64(X);
}
