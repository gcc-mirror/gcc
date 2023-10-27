/* Test specification of custom instructions via pragmas.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only -save-temps" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

#pragma GCC target ("custom-fmaxs=246")
#pragma GCC target ("custom-fmins=247")
#pragma GCC target ("custom-fsqrts=251")

void
custom_fp (float operand_a, float operand_b, float *result)
{
  result[0] = fmaxf (operand_a, operand_b);
  result[1] = fminf (operand_a, operand_b);
  result[2] = sqrtf (operand_a);
}

/* { dg-final { scan-assembler "custom\\t246, .* # fmaxs .*" } } */
/* { dg-final { scan-assembler "custom\\t247, .* # fmins .*" } } */
/* { dg-final { scan-assembler "custom\\t251, .* # fsqrts .*" } } */
