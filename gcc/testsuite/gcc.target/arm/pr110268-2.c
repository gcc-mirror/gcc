/* { dg-do link }  */
/* { dg-require-effective-target arm_arch_v8_1m_main_link } */ /* Make sure we have suitable multilibs to link successfully.  */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2 -flto" } */

/* Check MVE intrinsics with LTO with __ARM_MVE_PRESERVE_USER_NAMESPACE and a
   user-overridden intrinsic.  */

#define __ARM_MVE_PRESERVE_USER_NAMESPACE
#include <arm_mve.h>

int global_int;
int32_t vaddvq(int8x16_t x)
{
  return global_int + __arm_vgetq_lane_s8 (x, 0);
}

int main(int argc, char* argv[])
{
  global_int = argc;
  return vaddvq(__arm_vdupq_n_s8 (argc));
}
