/* Test that you cannot inline a function with custom insn attributes into
   one with incompatible attributes.  */

/* { dg-do compile } */
/* { dg-options "-O1 -ffinite-math-only" } */

/* -O1 in the options is significant.  Without it FP operations may not be
   optimized to custom instructions.  */

#include <math.h>

static inline
__attribute__ ((always_inline, target ("custom-fmaxs=246,custom-fmins=247")))
void
custom_fp1 (float operand_a, float operand_b, float *result)  /* { dg-error "target specific option mismatch" } */
{
  result[0] = fmaxf (operand_a, operand_b);
  result[1] = fminf (operand_a, operand_b);
}

extern void
custom_fp (float operand_a, float operand_b, float *result)
  __attribute__ ((target ("custom-fmaxs=200,custom-fmins=201")));

void
custom_fp (float operand_a, float operand_b, float *result)
{
  custom_fp1 (operand_a, operand_b, result);
}
