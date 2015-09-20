/* Verify that the fmac insn is used for the standard fmaf function.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler "fmac" } } */

#include <math.h>

float
test_func_00 (float a, float b, float c)
{
  return fmaf (a, b, c);
}

