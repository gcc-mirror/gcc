/* Test conflict between pragma and attribute specification of custom
   instructions.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <stdio.h> 
#include <math.h>

/* This test case is expected to cause an error because GCC does not know
   how to merge different custom instruction attribute sets.  The extern
   declaration sees the options specified by both the pragma and the function
   attribute, but the function definition sees only the pragma options.  */

#pragma GCC target ("custom-fmaxs=246")

extern void
custom_fp (float operand_a, float operand_b, float *result)
  __attribute__ ((target ("custom-fmins=247")));

void
custom_fp (float operand_a, float operand_b, float *result)
{   /* { dg-error "conflicting" } */
  result[0] = fmaxf (operand_a, operand_b);
  result[1] = fminf (operand_a, operand_b);
}
