/* { dg-do assemble }  */
/* { dg-require-effective-target arm_neon }  */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_neon }  */

#include <arm_neon.h>

int32_t a __attribute__ ((aligned (64)));

int32x2x3_t test (void)
{
  return vld3_dup_s32 (&a);
}

int32x2x3_t test1 (void)
{
  int32x2x3_t res ;
  return vld3_lane_s32 (&a, res, 1);
}

