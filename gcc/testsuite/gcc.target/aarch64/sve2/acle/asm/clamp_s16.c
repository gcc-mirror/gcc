/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** clamp_s16_tied1:
**	sclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_s16_tied1, svint16_t,
		z0 = svclamp_s16 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_s16_tied2:
**	sclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_s16_tied2, svint16_t,
		z0 = svclamp_s16 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sclamp	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_s16_tied3, svint16_t,
		z0 = svclamp_s16 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_s16_untied:
**	movprfx	z0, z1
**	sclamp	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_s16_untied, svint16_t,
		z0 = svclamp_s16 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
