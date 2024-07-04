/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** clamp_s32_tied1:
**	sclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_s32_tied1, svint32_t,
		z0 = svclamp_s32 (z0, z1, z2),
		z0 = svclamp (z0, z1, z2))

/*
** clamp_s32_tied2:
**	sclamp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_s32_tied2, svint32_t,
		z0 = svclamp_s32 (z1, z0, z2),
		z0 = svclamp (z1, z0, z2))

/*
** clamp_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sclamp	z0\.s, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_s32_tied3, svint32_t,
		z0 = svclamp_s32 (z1, z2, z0),
		z0 = svclamp (z1, z2, z0))

/*
** clamp_s32_untied:
**	movprfx	z0, z1
**	sclamp	z0\.s, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (clamp_s32_untied, svint32_t,
		z0 = svclamp_s32 (z1, z2, z3),
		z0 = svclamp (z1, z2, z3))
