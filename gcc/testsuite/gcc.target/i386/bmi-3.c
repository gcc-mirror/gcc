/* { dg-do compile } */
/* { dg-options "-O2 -mbmi " } */
/* { dg-final { scan-assembler-times "tzcntw\[^\\n]*%?ax" 2 } } */

#include <x86intrin.h>

unsigned short
func_tzcnt16 (unsigned short X)
{
  return __tzcnt_u16(X);
}

unsigned short
func_tzcnt16_2 (unsigned short X)
{
  return _tzcnt_u16(X);
}
