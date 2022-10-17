/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -ffast-math" } */

#include<math.h>
_Float16 foo1 (_Float16 a)
{
  return sqrtf (a);
}

_Float16 foo2 (_Float16 a)
{
  return sqrt (a);
}

_Float16 foo3 (_Float16 a)
{
  return sqrtl (a);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } } */
/* { dg-final { scan-assembler-not "extendhfxf" } } */
/* { dg-final { scan-assembler-times "vsqrtsh\[^\n\r\]*xmm\[0-9\]" 3 } } */
