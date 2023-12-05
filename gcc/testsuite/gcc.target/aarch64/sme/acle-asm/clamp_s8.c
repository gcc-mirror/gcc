/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** clamp_s8_tied1:
**	sclamp	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (clamp_s8_tied1, svint8_t,
		z0 = svclamp_s8 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_s8_tied2:
**	sclamp	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (clamp_s8_tied2, svint8_t,
		z0 = svclamp_s8 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sclamp	z0\.b, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (clamp_s8_tied3, svint8_t,
		z0 = svclamp_s8 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_s8_untied:
**	movprfx	z0, z1
**	sclamp	z0\.b, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (clamp_s8_untied, svint8_t,
		z0 = svclamp_s8 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
