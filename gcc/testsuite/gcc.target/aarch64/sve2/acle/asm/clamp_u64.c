/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** clamp_u64_tied1:
**	uclamp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_u64_tied1, svuint64_t,
		z0 = svclamp_u64 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_u64_tied2:
**	uclamp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_u64_tied2, svuint64_t,
		z0 = svclamp_u64 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uclamp	z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_u64_tied3, svuint64_t,
		z0 = svclamp_u64 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_u64_untied:
**	movprfx	z0, z1
**	uclamp	z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (clamp_u64_untied, svuint64_t,
		z0 = svclamp_u64 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
