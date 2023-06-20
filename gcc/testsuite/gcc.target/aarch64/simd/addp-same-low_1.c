/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>


/*
** foo:
**	addp	v0\.4s, v0\.4s, v0\.4s
**	ret
*/

int32x2_t
foo (int32x4_t a)
{
  return vpadd_s32 (vget_low_s32(a), vget_high_s32(a));
}

