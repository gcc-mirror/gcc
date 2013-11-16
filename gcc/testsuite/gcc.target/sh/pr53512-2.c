/* Verify that the fsca insn is not used when specifying -mno-fsca and
  -funsafe-math-optimizations.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mno-fsca -funsafe-math-optimizations" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2*" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-not "fsca" } } */

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
