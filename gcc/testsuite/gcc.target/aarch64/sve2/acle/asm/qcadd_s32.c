/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qcadd_90_s32_tied1:
**	sqcadd	z0\.s, z0\.s, z1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s32_tied1, svint32_t,
		z0 = svqcadd_s32 (z0, z1, 90),
		z0 = svqcadd (z0, z1, 90))

/*
** qcadd_90_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.s, z0\.s, \1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s32_tied2, svint32_t,
		z0 = svqcadd_s32 (z1, z0, 90),
		z0 = svqcadd (z1, z0, 90))

/*
** qcadd_90_s32_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.s, z0\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s32_untied, svint32_t,
		z0 = svqcadd_s32 (z1, z2, 90),
		z0 = svqcadd (z1, z2, 90))

/*
** qcadd_270_s32_tied1:
**	sqcadd	z0\.s, z0\.s, z1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s32_tied1, svint32_t,
		z0 = svqcadd_s32 (z0, z1, 270),
		z0 = svqcadd (z0, z1, 270))

/*
** qcadd_270_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.s, z0\.s, \1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s32_tied2, svint32_t,
		z0 = svqcadd_s32 (z1, z0, 270),
		z0 = svqcadd (z1, z0, 270))

/*
** qcadd_270_s32_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.s, z0\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s32_untied, svint32_t,
		z0 = svqcadd_s32 (z1, z2, 270),
		z0 = svqcadd (z1, z2, 270))
