/* Verify that the fsrra insn is not used when specifying -mno-fsrra and
  -funsafe-math-optimizations and -ffinite-math-only.  */
/* { dg-do compile { target { has_fsrra } } }  */
/* { dg-options "-O1 -mno-fsrra -funsafe-math-optimizations -ffinite-math-only -fno-math-errno" } */
/* { dg-final { scan-assembler-not "fsrra" } } */

#include <math.h>

float
test_func_00 (float x)
{
  return 1 / sqrtf (x);
}
