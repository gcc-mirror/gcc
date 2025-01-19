/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** pmov_to_vector:
**	pmov	z0, p0\.s
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector, svuint32_t,
		z0 = svpmov_u32_z (p0),
		z0 = svpmov_u32_z (p0));

/*
** pmov_to_vector_1_tied:
**	pmov	z0\[1\], p0\.s
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_1_tied, svuint32_t,
		z0 = svpmov_lane_u32_m (z0, p0, 1),
		z0 = svpmov_lane_m (z0, p0, 1));

/*
** pmov_to_vector_1_untied:
** (
**	mov	z0\.d, z1\.d
**	pmov	z0\[1\], p0\.s
** |
**	pmov	z1\[1\], p0\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_1_untied, svuint32_t,
		z0 = svpmov_lane_u32_m (z1, p0, 1),
		z0 = svpmov_lane_m (z1, p0, 1));

/*
** pmov_to_vector_2:
**	pmov	z0\[2\], p0\.s
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_2, svuint32_t,
		z0 = svpmov_lane_u32_m (z0, p0, 2),
		z0 = svpmov_lane_m (z0, p0, 2));

/*
** pmov_to_vector_3:
**	pmov	z0\[3\], p0\.s
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector_3, svuint32_t,
		z0 = svpmov_lane_u32_m (z0, p0, 3),
		z0 = svpmov_lane_m (z0, p0, 3));

/*
** pmov_from_vector:
**	pmov	p0\.s, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector, svuint32_t,
		p0 = svpmov_u32 (z0),
		p0 = svpmov (z0));

/*
** pmov_from_vector_0:
**	pmov	p0\.s, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_0, svuint32_t,
		p0 = svpmov_lane_u32 (z0, 0),
		p0 = svpmov_lane (z0, 0));

/*
** pmov_from_vector_1:
**	pmov	p0\.s, z0\[1\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_1, svuint32_t,
		p0 = svpmov_lane_u32 (z0, 1),
		p0 = svpmov_lane (z0, 1));

/*
** pmov_from_vector_2:
**	pmov	p0\.s, z0\[2\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_2, svuint32_t,
		p0 = svpmov_lane_u32 (z0, 2),
		p0 = svpmov_lane (z0, 2));

/*
** pmov_from_vector_3:
**	pmov	p0\.s, z0\[3\]
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_3, svuint32_t,
		p0 = svpmov_lane_u32 (z0, 3),
		p0 = svpmov_lane (z0, 3));
