/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mbig_endian } */
/* Avoid a dependence on big-endian C library headers.  */
/* { dg-options "-O2 -mbig-endian -ffreestanding" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/* The architectural lanes of the low half depend only on A.  */

/*
** lane0_q:
**	addp	(v[0-9]+)\.4s, v0\.4s, v0\.4s
**	...
*/
int32_t
lane0_q (int32x4_t a, int32x4_t b)
{
  return vgetq_lane_s32 (vpaddq_s32 (a, b), 0);
}

/*
** low_q:
**	addp	v0\.4s, v0\.4s, v0\.4s
**	ret
*/
int32x2_t
low_q (int32x4_t a, int32x4_t b)
{
  return vget_low_s32 (vpaddq_s32 (a, b));
}
