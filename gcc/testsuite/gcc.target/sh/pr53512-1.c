/* Verify that the fsca insn is used when specifying -mfsca and
  -funsafe-math-optimizations.  */
/* { dg-do compile { target { has_fsca } } }  */
/* { dg-options "-O1 -mfsca -funsafe-math-optimizations -fno-math-errno" } */
/* { dg-final { scan-assembler-times "fsca" 3 } } */

#include <math.h>

float
test_func_00 (float x)
{
  return sinf (x) + cosf (x);
}

float
test_func_01 (float x)
{
  return sinf (x);
}

float
test_func_02 (float x)
{
  return cosf (x);
}
