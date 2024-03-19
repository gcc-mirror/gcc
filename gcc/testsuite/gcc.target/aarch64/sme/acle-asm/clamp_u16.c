/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** clamp_u16_tied1:
**	uclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_u16_tied1, svuint16_t,
		z0 = svclamp_u16 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_u16_tied2:
**	uclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_u16_tied2, svuint16_t,
		z0 = svclamp_u16 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_u16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uclamp	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_u16_tied3, svuint16_t,
		z0 = svclamp_u16 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_u16_untied:
**	movprfx	z0, z1
**	uclamp	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_u16_untied, svuint16_t,
		z0 = svclamp_u16 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
