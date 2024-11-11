/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** pmov_to_vector:
**	pmov	z0, p0\.d
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector, svint64_t,
		z0 = svpmov_s64_z (p0),
		z0 = svpmov_s64_z (p0));

/*
** pmov_to_vector_1_tied:
**	pmov	z0\[1\], p0\.d
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_1_tied, svint64_t,
		z0 = svpmov_lane_s64_m (z0, p0, 1),
		z0 = svpmov_lane_m (z0, p0, 1));

/*
** pmov_to_vector_4_untied:
** (
**	mov	z0\.d, z1\.d
**	pmov	z0\[4\], p0\.d
** |
**	pmov	z1\[4\], p0\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_4_untied, svint64_t,
		z0 = svpmov_lane_s64_m (z1, p0, 4),
		z0 = svpmov_lane_m (z1, p0, 4));

/*
** pmov_to_vector_6:
**	pmov	z0\[6\], p0\.d
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_6, svint64_t,
		z0 = svpmov_lane_s64_m (z0, p0, 6),
		z0 = svpmov_lane_m (z0, p0, 6));

/*
** pmov_to_vector_7:
**	pmov	z0\[7\], p0\.d
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_7, svint64_t,
		z0 = svpmov_lane_s64_m (z0, p0, 7),
		z0 = svpmov_lane_m (z0, p0, 7));

/*
** pmov_from_vector:
**	pmov	p0\.d, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector, svint64_t,
		p0 = svpmov_s64 (z0),
		p0 = svpmov (z0));

/*
** pmov_from_vector_0:
**	pmov	p0\.d, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_0, svint64_t,
		p0 = svpmov_lane_s64 (z0, 0),
		p0 = svpmov_lane (z0, 0));

/*
** pmov_from_vector_4:
**	pmov	p0\.d, z0\[4\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_4, svint64_t,
		p0 = svpmov_lane_s64 (z0, 4),
		p0 = svpmov_lane (z0, 4));

/*
** pmov_from_vector_5:
**	pmov	p0\.d, z0\[5\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_5, svint64_t,
		p0 = svpmov_lane_s64 (z0, 5),
		p0 = svpmov_lane (z0, 5));

/*
** pmov_from_vector_7:
**	pmov	p0\.d, z0\[7\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_7, svint64_t,
		p0 = svpmov_lane_s64 (z0, 7),
		p0 = svpmov_lane (z0, 7));
