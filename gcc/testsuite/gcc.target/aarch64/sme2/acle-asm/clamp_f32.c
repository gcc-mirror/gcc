/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** clamp_f32_tied1:
**	fclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_f32_tied1, svfloat32_t,
		z0 = svclamp_f32 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_f32_tied2:
**	fclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_f32_tied2, svfloat32_t,
		z0 = svclamp_f32 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fclamp	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_f32_tied3, svfloat32_t,
		z0 = svclamp_f32 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_f32_untied:
**	movprfx	z0, z1
**	fclamp	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_f32_untied, svfloat32_t,
		z0 = svclamp_f32 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
