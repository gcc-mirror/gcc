/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_f16_tied1:
**	fclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_f16_tied1, svfloat16_t,
		z0 = svclamp_f16 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_f16_tied2:
**	fclamp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_f16_tied2, svfloat16_t,
		z0 = svclamp_f16 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_f16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fclamp	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_f16_tied3, svfloat16_t,
		z0 = svclamp_f16 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_f16_untied:
**	movprfx	z0, z1
**	fclamp	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (clamp_f16_untied, svfloat16_t,
		z0 = svclamp_f16 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
