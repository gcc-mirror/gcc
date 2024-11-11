/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** extq_1_s8_tied1:
**	extq	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (extq_1_s8_tied1, svint8_t,
		z0 = svextq_s8 (z0, z1, 1),
		z0 = svextq (z0, z1, 1))

/*
** extq_1_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, \1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (extq_1_s8_tied2, svint8_t,
		z0 = svextq_s8 (z1, z0, 1),
		z0 = svextq (z1, z0, 1))

/*
** extq_1_s8_untied:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #1
**	ret
*/
TEST_UNIFORM_Z (extq_1_s8_untied, svint8_t,
		z0 = svextq_s8 (z1, z2, 1),
		z0 = svextq (z1, z2, 1))

/*
** extq_0_s8:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (extq_0_s8, svint8_t,
		z0 = svextq_s8 (z1, z2, 0),
		z0 = svextq (z1, z2, 0))

/*
** extq_2_s8:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #2
**	ret
*/
TEST_UNIFORM_Z (extq_2_s8, svint8_t,
		z0 = svextq_s8 (z1, z2, 2),
		z0 = svextq (z1, z2, 2))

/*
** extq_3_s8:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #3
**	ret
*/
TEST_UNIFORM_Z (extq_3_s8, svint8_t,
		z0 = svextq_s8 (z1, z2, 3),
		z0 = svextq (z1, z2, 3))

/*
** extq_15_s8:
**	movprfx	z0, z1
**	extq	z0\.b, z0\.b, z2\.b, #15
**	ret
*/
TEST_UNIFORM_Z (extq_15_s8, svint8_t,
		z0 = svextq_s8 (z1, z2, 15),
		z0 = svextq (z1, z2, 15))
