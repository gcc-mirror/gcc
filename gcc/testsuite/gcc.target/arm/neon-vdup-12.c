/* Test the optimization of `vdupq_n_u16' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint16x8_t out_uint16x8_t;
void test_vdupq_nu16 (void)
{
  out_uint16x8_t = vdupq_n_u16 (0x1200);
}

/* { dg-final { scan-assembler "vmov\.i16\[ 	\]+\[qQ\]\[0-9\]+, #4608\(\[ \]+@\[a-zA-Z0-9 \]+\)?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
