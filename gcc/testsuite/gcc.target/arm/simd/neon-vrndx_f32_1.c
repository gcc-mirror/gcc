/* Test the `vrndx_f32' ARM Neon intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_neon } */

#include "arm_neon.h"

float32x2_t
test_vrndx_f32 (float32x2_t in)
{
  return vrndx_f32 (in);
}

/* { dg-final { scan-assembler "vrintx\.f32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+\n" } } */
