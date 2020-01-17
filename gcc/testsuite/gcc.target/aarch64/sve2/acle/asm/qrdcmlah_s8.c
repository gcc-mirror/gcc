/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdcmlah_0_s8_tied1:
**	sqrdcmlah	z0\.b, z1\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s8_tied1, svint8_t,
		z0 = svqrdcmlah_s8 (z0, z1, z2, 0),
		z0 = svqrdcmlah (z0, z1, z2, 0))

/*
** qrdcmlah_0_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, \1\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s8_tied2, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z0, z2, 0),
		z0 = svqrdcmlah (z1, z0, z2, 0))

/*
** qrdcmlah_0_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, \1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s8_tied3, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z0, 0),
		z0 = svqrdcmlah (z1, z2, z0, 0))

/*
** qrdcmlah_0_s8_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, z3\.b, #0
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_0_s8_untied, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z3, 0),
		z0 = svqrdcmlah (z1, z2, z3, 0))

/*
** qrdcmlah_90_s8_tied1:
**	sqrdcmlah	z0\.b, z1\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s8_tied1, svint8_t,
		z0 = svqrdcmlah_s8 (z0, z1, z2, 90),
		z0 = svqrdcmlah (z0, z1, z2, 90))

/*
** qrdcmlah_90_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, \1\.b, z2\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s8_tied2, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z0, z2, 90),
		z0 = svqrdcmlah (z1, z0, z2, 90))

/*
** qrdcmlah_90_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, \1\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s8_tied3, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z0, 90),
		z0 = svqrdcmlah (z1, z2, z0, 90))

/*
** qrdcmlah_90_s8_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, z3\.b, #90
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_90_s8_untied, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z3, 90),
		z0 = svqrdcmlah (z1, z2, z3, 90))

/*
** qrdcmlah_180_s8_tied1:
**	sqrdcmlah	z0\.b, z1\.b, z2\.b, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s8_tied1, svint8_t,
		z0 = svqrdcmlah_s8 (z0, z1, z2, 180),
		z0 = svqrdcmlah (z0, z1, z2, 180))

/*
** qrdcmlah_180_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, \1\.b, z2\.b, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s8_tied2, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z0, z2, 180),
		z0 = svqrdcmlah (z1, z0, z2, 180))

/*
** qrdcmlah_180_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, \1\.b, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s8_tied3, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z0, 180),
		z0 = svqrdcmlah (z1, z2, z0, 180))

/*
** qrdcmlah_180_s8_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, z3\.b, #180
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_180_s8_untied, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z3, 180),
		z0 = svqrdcmlah (z1, z2, z3, 180))

/*
** qrdcmlah_270_s8_tied1:
**	sqrdcmlah	z0\.b, z1\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s8_tied1, svint8_t,
		z0 = svqrdcmlah_s8 (z0, z1, z2, 270),
		z0 = svqrdcmlah (z0, z1, z2, 270))

/*
** qrdcmlah_270_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, \1\.b, z2\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s8_tied2, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z0, z2, 270),
		z0 = svqrdcmlah (z1, z0, z2, 270))

/*
** qrdcmlah_270_s8_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, \1\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s8_tied3, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z0, 270),
		z0 = svqrdcmlah (z1, z2, z0, 270))

/*
** qrdcmlah_270_s8_untied:
**	movprfx	z0, z1
**	sqrdcmlah	z0\.b, z2\.b, z3\.b, #270
**	ret
*/
TEST_UNIFORM_Z (qrdcmlah_270_s8_untied, svint8_t,
		z0 = svqrdcmlah_s8 (z1, z2, z3, 270),
		z0 = svqrdcmlah (z1, z2, z3, 270))
