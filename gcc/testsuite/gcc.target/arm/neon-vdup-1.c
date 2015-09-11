/* Test the optimization of `vdupq_n_f32' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

float32x4_t out_float32x4_t;
void test_vdupq_nf32 (void)
{
  out_float32x4_t = vdupq_n_f32 (0.0);
}

/* { dg-final { scan-assembler "vmov\.f32\[ 	\]+\[qQ\]\[0-9\]+, #0\.0\(\[ \]+@\[a-zA-Z0-9 \]+\)?\n" } } */
