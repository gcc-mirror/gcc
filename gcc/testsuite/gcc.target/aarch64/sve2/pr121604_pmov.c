/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve2p1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** f:
**	pfalse	p([0-7]+)\.b
**	mov	z0\.b, #-1
**	pmov	z0\[1\], p\1\.d
**	ret
*/
svuint64_t f () {
    return svpmov_lane_u64_m (svdup_u64 (~0UL), svpfalse (), 1);
}
