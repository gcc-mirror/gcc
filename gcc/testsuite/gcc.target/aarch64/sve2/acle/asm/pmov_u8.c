/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** pmov_to_vector:
**	pmov	z0, p0\.b
**	ret
*/
TEST_UNIFORM_Z (pmov_to_vector, svuint8_t,
		z0 = svpmov_u8_z (p0),
		z0 = svpmov_u8_z (p0));

/*
** pmov_from_vector:
**	pmov	p0\.b, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector, svuint8_t,
		p0 = svpmov_u8 (z0),
		p0 = svpmov (z0));

/*
** pmov_from_vector_0:
**	pmov	p0\.b, z0
**	ret
*/
TEST_COMPARE_Z (pmov_from_vector_0, svuint8_t,
		p0 = svpmov_lane_u8 (z0, 0),
		p0 = svpmov_lane (z0, 0));
