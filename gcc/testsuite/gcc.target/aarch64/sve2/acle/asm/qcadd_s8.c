/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qcadd_90_s8_tied1:
**	sqcadd	z0\.b, z0\.b, z1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s8_tied1, svint8_t,
		z0 = svqcadd_s8 (z0, z1, 90),
		z0 = svqcadd (z0, z1, 90))

/*
** qcadd_90_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.b, z0\.b, \1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s8_tied2, svint8_t,
		z0 = svqcadd_s8 (z1, z0, 90),
		z0 = svqcadd (z1, z0, 90))

/*
** qcadd_90_s8_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.b, z0\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s8_untied, svint8_t,
		z0 = svqcadd_s8 (z1, z2, 90),
		z0 = svqcadd (z1, z2, 90))

/*
** qcadd_270_s8_tied1:
**	sqcadd	z0\.b, z0\.b, z1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s8_tied1, svint8_t,
		z0 = svqcadd_s8 (z0, z1, 270),
		z0 = svqcadd (z0, z1, 270))

/*
** qcadd_270_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.b, z0\.b, \1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s8_tied2, svint8_t,
		z0 = svqcadd_s8 (z1, z0, 270),
		z0 = svqcadd (z1, z0, 270))

/*
** qcadd_270_s8_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.b, z0\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s8_untied, svint8_t,
		z0 = svqcadd_s8 (z1, z2, 270),
		z0 = svqcadd (z1, z2, 270))
