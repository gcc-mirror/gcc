/* Test the optimization of `vdupq_n_u8' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint8x16_t out_uint8x16_t;
void test_vdupq_nu8 (void)
{
  out_uint8x16_t = vdupq_n_u8 (0x12);
}

/* { dg-final { scan-assembler "vmov\.i8\[ 	\]+\[qQ\]\[0-9\]+, #18\(\[ \]+@\[a-zA-Z0-9 \]+\)?\n" } } */
