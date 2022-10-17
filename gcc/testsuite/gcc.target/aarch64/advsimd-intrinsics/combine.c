/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
** foo:
**	umov	w0, v1\.s\[1\]
**	ret
*/

int32_t foo (int32x2_t a, int32x2_t b)
{
  int32x4_t c = vcombine_s32(a, b);
  return vgetq_lane_s32(c, 3);
}

