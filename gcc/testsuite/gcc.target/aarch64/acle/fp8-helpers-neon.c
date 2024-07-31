/* Test the fp8 ACLE helper functions including that they are available.
   unconditionally when including arm_neon.h */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors -O1 -march=armv8-a" } */

#include <arm_neon.h>

void
test_prepare_fpmr_sysreg ()
{

#define _S_EQ(expr, expected)                                                  \
  _Static_assert (expr == expected, #expr " == " #expected)

  _S_EQ (__arm_fpm_init (), 0);

  /* Bits [2:0] */
  _S_EQ (__arm_set_fpm_src1_format (__arm_fpm_init (), __ARM_FPM_E5M2), 0);
  _S_EQ (__arm_set_fpm_src1_format (__arm_fpm_init (), __ARM_FPM_E4M3), 0x1);

  /* Bits [5:3] */
  _S_EQ (__arm_set_fpm_src2_format (__arm_fpm_init (), __ARM_FPM_E5M2), 0);
  _S_EQ (__arm_set_fpm_src2_format (__arm_fpm_init (), __ARM_FPM_E4M3), 0x8);

  /* Bits [8:6] */
  _S_EQ (__arm_set_fpm_dst_format (__arm_fpm_init (), __ARM_FPM_E5M2), 0);
  _S_EQ (__arm_set_fpm_dst_format (__arm_fpm_init (), __ARM_FPM_E4M3), 0x40);

  /* Bit 14 */
  _S_EQ (__arm_set_fpm_overflow_mul (__arm_fpm_init (), __ARM_FPM_INFNAN), 0);
  _S_EQ (__arm_set_fpm_overflow_mul (__arm_fpm_init (), __ARM_FPM_SATURATE),
	 0x4000);

  /* Bit 15 */
  _S_EQ (__arm_set_fpm_overflow_cvt (__arm_fpm_init (), __ARM_FPM_INFNAN), 0);
  _S_EQ (__arm_set_fpm_overflow_cvt (__arm_fpm_init (), __ARM_FPM_SATURATE),
	 0x8000);

  /* Bits [22:16] */
  _S_EQ (__arm_set_fpm_lscale (__arm_fpm_init (), 0), 0);
  _S_EQ (__arm_set_fpm_lscale (__arm_fpm_init (), 127), 0x7F0000);

  /* Bits [37:32] */
  _S_EQ (__arm_set_fpm_lscale2 (__arm_fpm_init (), 0), 0);
  _S_EQ (__arm_set_fpm_lscale2 (__arm_fpm_init (), 63), 0x3F00000000);

  /* Bits [31:24] */
  _S_EQ (__arm_set_fpm_nscale (__arm_fpm_init (), 0), 0);
  _S_EQ (__arm_set_fpm_nscale (__arm_fpm_init (), 127), 0x7F000000);
  _S_EQ (__arm_set_fpm_nscale (__arm_fpm_init (), -128), 0x80000000);

#undef _S_EQ
}
