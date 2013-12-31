/* Test that duplicate declarations with the same custom insn attributes
   don't cause an error.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

/* This test case is expected to cause an error because GCC does not know
   how to merge different custom instruction attribute sets, even if they
   do not overlap.  */

extern void
custom_fp (float operand_a, float operand_b, float *result)
  __attribute__ ((target ("custom-fmaxs=246,custom-fmins=247")));

extern void
custom_fp (float operand_a, float operand_b, float *result)
  __attribute__ ((target ("custom-fmaxs=246,custom-fmins=247")));

void
custom_fp (float operand_a, float operand_b, float *result)
{
  result[0] = fmaxf (operand_a, operand_b);
  result[1] = fminf (operand_a, operand_b);
}

/* { dg-final { scan-assembler "custom\\t246, .* # fmaxs .*" } } */
/* { dg-final { scan-assembler "custom\\t247, .* # fmins .*" } } */
