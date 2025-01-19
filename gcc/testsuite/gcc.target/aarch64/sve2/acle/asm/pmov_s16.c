/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** pmov_to_vector:
**	pmov	z0, p0\.h
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector, svint16_t,
		z0 = svpmov_s16_z (p0),
		z0 = svpmov_s16_z (p0));

/*
** pmov_to_vector_1_tied:
**	pmov	z0\[1\], p0\.h
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_1_tied, svint16_t,
		z0 = svpmov_lane_s16_m (z0, p0, 1),
		z0 = svpmov_lane_m (z0, p0, 1));

/*
** pmov_to_vector_1_untied:
** (
**	mov	z0\.d, z1\.d
**	pmov	z0\[1\], p0\.h
** |
**	pmov	z1\[1\], p0\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_1_untied, svint16_t,
		z0 = svpmov_lane_s16_m (z1, p0, 1),
		z0 = svpmov_lane_m (z1, p0, 1));

/*
** pmov_from_vector:
**	pmov	p0\.h, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector, svint16_t,
		p0 = svpmov_s16 (z0),
		p0 = svpmov (z0));

/*
** pmov_from_vector_0:
**	pmov	p0\.h, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_0, svint16_t,
		p0 = svpmov_lane_s16 (z0, 0),
		p0 = svpmov_lane (z0, 0));

/*
** pmov_from_vector_1:
**	pmov	p0\.h, z0\[1\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_1, svint16_t,
		p0 = svpmov_lane_s16 (z0, 1),
		p0 = svpmov_lane (z0, 1));
