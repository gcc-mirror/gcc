/* Verify that the fsca insn is used when specifying -mfsca and
  -funsafe-math-optimizations.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mfsca -funsafe-math-optimizations" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2*" "-m3*" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
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
