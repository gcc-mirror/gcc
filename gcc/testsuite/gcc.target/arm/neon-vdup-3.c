/* Test the optimization of `vdupq_n_u32' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint32x4_t out_uint32x4_t;
void test_vdupq_nu32 (void)
{
  out_uint32x4_t = vdupq_n_u32 (0x12);
}

/* { dg-final { scan-assembler "vmov\.i32\[ 	\]+\[qQ\]\[0-9\]+, #18\(\[ \]+@\[a-zA-Z0-9 \]+\)?\n" } } */
