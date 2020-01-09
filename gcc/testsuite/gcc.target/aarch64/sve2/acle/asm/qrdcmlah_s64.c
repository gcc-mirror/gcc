/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdcmlah_0_s64_tied1:
**	sqrdcmlah	z0\.d, z1\.d, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s64_tied1, svint64_t,
		z0 = svqrdcmlah_s64 (z0, z1, z2, 0),
		z0 = svqrdcmlah (z0, z1, z2, 0))

/*
** qrdcmlah_0_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, \1, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s64_tied2, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z0, z2, 0),
		z0 = svqrdcmlah (z1, z0, z2, 0))

/*
** qrdcmlah_0_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, \1, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s64_tied3, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z0, 0),
		z0 = svqrdcmlah (z1, z2, z0, 0))

/*
** qrdcmlah_0_s64_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, z3\.d, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s64_untied, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z3, 0),
		z0 = svqrdcmlah (z1, z2, z3, 0))

/*
** qrdcmlah_90_s64_tied1:
**	sqrdcmlah	z0\.d, z1\.d, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s64_tied1, svint64_t,
		z0 = svqrdcmlah_s64 (z0, z1, z2, 90),
		z0 = svqrdcmlah (z0, z1, z2, 90))

/*
** qrdcmlah_90_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, \1, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s64_tied2, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z0, z2, 90),
		z0 = svqrdcmlah (z1, z0, z2, 90))

/*
** qrdcmlah_90_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, \1, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s64_tied3, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z0, 90),
		z0 = svqrdcmlah (z1, z2, z0, 90))

/*
** qrdcmlah_90_s64_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, z3\.d, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s64_untied, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z3, 90),
		z0 = svqrdcmlah (z1, z2, z3, 90))

/*
** qrdcmlah_180_s64_tied1:
**	sqrdcmlah	z0\.d, z1\.d, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s64_tied1, svint64_t,
		z0 = svqrdcmlah_s64 (z0, z1, z2, 180),
		z0 = svqrdcmlah (z0, z1, z2, 180))

/*
** qrdcmlah_180_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, \1, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s64_tied2, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z0, z2, 180),
		z0 = svqrdcmlah (z1, z0, z2, 180))

/*
** qrdcmlah_180_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, \1, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s64_tied3, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z0, 180),
		z0 = svqrdcmlah (z1, z2, z0, 180))

/*
** qrdcmlah_180_s64_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, z3\.d, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s64_untied, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z3, 180),
		z0 = svqrdcmlah (z1, z2, z3, 180))

/*
** qrdcmlah_270_s64_tied1:
**	sqrdcmlah	z0\.d, z1\.d, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s64_tied1, svint64_t,
		z0 = svqrdcmlah_s64 (z0, z1, z2, 270),
		z0 = svqrdcmlah (z0, z1, z2, 270))

/*
** qrdcmlah_270_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, \1, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s64_tied2, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z0, z2, 270),
		z0 = svqrdcmlah (z1, z0, z2, 270))

/*
** qrdcmlah_270_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, \1, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s64_tied3, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z0, 270),
		z0 = svqrdcmlah (z1, z2, z0, 270))

/*
** qrdcmlah_270_s64_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.d, z2\.d, z3\.d, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s64_untied, svint64_t,
		z0 = svqrdcmlah_s64 (z1, z2, z3, 270),
		z0 = svqrdcmlah (z1, z2, z3, 270))
