/* Verify that the fsrra insn is not used when specifying -mno-fsrra and
  -funsafe-math-optimizations and -ffinite-math-only.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1 -mno-fsrra -funsafe-math-optimizations -ffinite-math-only" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2*" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-not "fsrra" } } */

#include <math.h>

float
test_func_00 (float x)
{
  return 1 / sqrtf (x);
}

