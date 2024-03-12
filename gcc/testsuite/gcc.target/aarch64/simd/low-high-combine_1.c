/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** foo_le:	{ target aarch64_little_endian }
**	ret
*/

int32x4_t
foo_le (int32x4_t a)
{
  return vcombine_s32 (vget_low_s32 (a), vget_high_s32 (a));
}

/*
** foo_be:	{ target aarch64_big_endian }
**	ret
*/

int32x4_t
foo_be (int32x4_t a)
{
  return vcombine_s32 (vget_high_s32 (a), vget_low_s32 (a));
}

