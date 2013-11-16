/* Verify that the fmac insn is used for the standard fmaf function.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler "fmac" } } */

#include <math.h>

float
test_func_00 (float a, float b, float c)
{
  return fmaf (a, b, c);
}

