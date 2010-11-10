/* { dg-do compile } */
/* { dg-options "-O2 -mbmi " } */
/* { dg-final { scan-assembler "tzcntw\[^\\n]*(%|)ax" } } */

#include <x86intrin.h>

unsigned short
func_tzcnt16 (unsigned short X)
{
  return __tzcnt_u16(X);
}
