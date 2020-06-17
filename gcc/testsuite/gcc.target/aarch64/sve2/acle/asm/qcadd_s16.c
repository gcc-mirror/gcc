/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qcadd_90_s16_tied1:
**	sqcadd	z0\.h, z0\.h, z1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s16_tied1, svint16_t,
		z0 = svqcadd_s16 (z0, z1, 90),
		z0 = svqcadd (z0, z1, 90))

/*
** qcadd_90_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.h, z0\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s16_tied2, svint16_t,
		z0 = svqcadd_s16 (z1, z0, 90),
		z0 = svqcadd (z1, z0, 90))

/*
** qcadd_90_s16_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.h, z0\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s16_untied, svint16_t,
		z0 = svqcadd_s16 (z1, z2, 90),
		z0 = svqcadd (z1, z2, 90))

/*
** qcadd_270_s16_tied1:
**	sqcadd	z0\.h, z0\.h, z1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s16_tied1, svint16_t,
		z0 = svqcadd_s16 (z0, z1, 270),
		z0 = svqcadd (z0, z1, 270))

/*
** qcadd_270_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.h, z0\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s16_tied2, svint16_t,
		z0 = svqcadd_s16 (z1, z0, 270),
		z0 = svqcadd (z1, z0, 270))

/*
** qcadd_270_s16_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.h, z0\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s16_untied, svint16_t,
		z0 = svqcadd_s16 (z1, z2, 270),
		z0 = svqcadd (z1, z2, 270))
