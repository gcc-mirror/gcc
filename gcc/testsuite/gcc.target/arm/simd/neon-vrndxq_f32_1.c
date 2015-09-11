/* Test the `vrndxq_f32' ARM Neon intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_neon } */

#include "arm_neon.h"

float32x4_t
test_vrndxq_f32 (float32x4_t in)
{
  return vrndxq_f32 (in);
}

/* { dg-final { scan-assembler "vrintx\.f32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+\n" } } */
