/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-additional-options "-mfloat-abi=hard -O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**fn1:
**	vmov	d0, r0, r1
**	bx	lr
*/
uint64x2_t
fn1 (uint64_t a, uint64x2_t b)
{
  return vsetq_lane_u64 (a, b, 0);
}

/*
**fn2:
**	vmov	d1, r0, r1
**	bx	lr
*/
uint64x2_t
fn2 (uint64_t a, uint64x2_t b)
{
  return vsetq_lane_u64 (a, b, 1);
}

/*
**fn3:
**	vmov	d0, r0, r1
**	bx	lr
*/
int64x2_t
fn3 (int64_t a, int64x2_t b)
{
  return vsetq_lane_s64 (a, b, 0);
}

/*
**fn4:
**	vmov	d1, r0, r1
**	bx	lr
*/
int64x2_t
fn4 (int64_t a, int64x2_t b)
{
  return vsetq_lane_s64 (a, b, 1);
}


#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */

