/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** extq_1_u16_tied1:
**	extq	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (extq_1_u16_tied1, svuint16_t,
		z0 = svextq_u16 (z0, z1, 1),
		z0 = svextq (z0, z1, 1))

/*
** extq_1_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, \1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (extq_1_u16_tied2, svuint16_t,
		z0 = svextq_u16 (z1, z0, 1),
		z0 = svextq (z1, z0, 1))

/*
** extq_1_u16_untied:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #2
**	ret
*/
TEST_UNIFORM_Z (extq_1_u16_untied, svuint16_t,
		z0 = svextq_u16 (z1, z2, 1),
		z0 = svextq (z1, z2, 1))

/*
** extq_0_u16:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extq_0_u16, svuint16_t,
		z0 = svextq_u16 (z1, z2, 0),
		z0 = svextq (z1, z2, 0))

/*
** extq_2_u16:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #4
**	ret
*/
TEST_UNIFORM_Z (extq_2_u16, svuint16_t,
		z0 = svextq_u16 (z1, z2, 2),
		z0 = svextq (z1, z2, 2))

/*
** extq_3_u16:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #6
**	ret
*/
TEST_UNIFORM_Z (extq_3_u16, svuint16_t,
		z0 = svextq_u16 (z1, z2, 3),
		z0 = svextq (z1, z2, 3))

/*
** extq_7_u16:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #14
**	ret
*/
TEST_UNIFORM_Z (extq_7_u16, svuint16_t,
		z0 = svextq_u16 (z1, z2, 7),
		z0 = svextq (z1, z2, 7))
