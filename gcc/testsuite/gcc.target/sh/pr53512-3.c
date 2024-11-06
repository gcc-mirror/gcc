/* Verify that the fsrra insn is used when specifying -mfsrra and
  -funsafe-math-optimizations and -ffinite-math-only.  */
/* { dg-do compile { target { has_fsrra } } }  */
/* { dg-options "-O1 -mfsrra -funsafe-math-optimizations -ffinite-math-only -fno-math-errno" } */
/* { dg-final { scan-assembler "fsrra" } } */

#include <math.h>

float
test_func_00 (float x)
{
  return 1 / sqrtf (x);
}

