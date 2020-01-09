/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qcadd_90_s64_tied1:
**	sqcadd	z0\.d, z0\.d, z1\.d, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s64_tied1, svint64_t,
		z0 = svqcadd_s64 (z0, z1, 90),
		z0 = svqcadd (z0, z1, 90))

/*
** qcadd_90_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.d, z0\.d, \1, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s64_tied2, svint64_t,
		z0 = svqcadd_s64 (z1, z0, 90),
		z0 = svqcadd (z1, z0, 90))

/*
** qcadd_90_s64_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.d, z0\.d, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (qcadd_90_s64_untied, svint64_t,
		z0 = svqcadd_s64 (z1, z2, 90),
		z0 = svqcadd (z1, z2, 90))

/*
** qcadd_270_s64_tied1:
**	sqcadd	z0\.d, z0\.d, z1\.d, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s64_tied1, svint64_t,
		z0 = svqcadd_s64 (z0, z1, 270),
		z0 = svqcadd (z0, z1, 270))

/*
** qcadd_270_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqcadd	z0\.d, z0\.d, \1, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s64_tied2, svint64_t,
		z0 = svqcadd_s64 (z1, z0, 270),
		z0 = svqcadd (z1, z0, 270))

/*
** qcadd_270_s64_untied:
**	movprfx	z0, z1
**	sqcadd	z0\.d, z0\.d, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (qcadd_270_s64_untied, svint64_t,
		z0 = svqcadd_s64 (z1, z2, 270),
		z0 = svqcadd (z1, z2, 270))
