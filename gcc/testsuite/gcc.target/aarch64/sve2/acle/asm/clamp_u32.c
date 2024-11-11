/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** clamp_u32_tied1:
**	uclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_u32_tied1, svuint32_t,
		z0 = svclamp_u32 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_u32_tied2:
**	uclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_u32_tied2, svuint32_t,
		z0 = svclamp_u32 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_u32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uclamp	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_u32_tied3, svuint32_t,
		z0 = svclamp_u32 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_u32_untied:
**	movprfx	z0, z1
**	uclamp	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_u32_untied, svuint32_t,
		z0 = svclamp_u32 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
