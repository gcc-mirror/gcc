/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmlah_s16_tied1:
**	sqrdmlah	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s16_tied1, svint16_t,
		z0 = svqrdmlah_s16 (z0, z1, z2),
		z0 = svqrdmlah (z0, z1, z2))

/*
** qrdmlah_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s16_tied2, svint16_t,
		z0 = svqrdmlah_s16 (z1, z0, z2),
		z0 = svqrdmlah (z1, z0, z2))

/*
** qrdmlah_s16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s16_tied3, svint16_t,
		z0 = svqrdmlah_s16 (z1, z2, z0),
		z0 = svqrdmlah (z1, z2, z0))

/*
** qrdmlah_s16_untied:
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s16_untied, svint16_t,
		z0 = svqrdmlah_s16 (z1, z2, z3),
		z0 = svqrdmlah (z1, z2, z3))

/*
** qrdmlah_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqrdmlah	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqrdmlah_n_s16 (z0, z1, x0),
		 z0 = svqrdmlah (z0, z1, x0))

/*
** qrdmlah_w0_s16_tied2:
**	mov	(z[0-9]+\.h), w0
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_w0_s16_tied2, svint16_t, int16_t,
		 z0 = svqrdmlah_n_s16 (z1, z0, x0),
		 z0 = svqrdmlah (z1, z0, x0))

/*
** qrdmlah_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqrdmlah_n_s16 (z1, z2, x0),
		 z0 = svqrdmlah (z1, z2, x0))

/*
** qrdmlah_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	sqrdmlah	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s16_tied1, svint16_t,
		z0 = svqrdmlah_n_s16 (z0, z1, 11),
		z0 = svqrdmlah (z0, z1, 11))

/*
** qrdmlah_11_s16_tied2:
**	mov	(z[0-9]+\.h), #11
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, \2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s16_tied2, svint16_t,
		z0 = svqrdmlah_n_s16 (z1, z0, 11),
		z0 = svqrdmlah (z1, z0, 11))

/*
** qrdmlah_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	movprfx	z0, z1
**	sqrdmlah	z0\.h, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s16_untied, svint16_t,
		z0 = svqrdmlah_n_s16 (z1, z2, 11),
		z0 = svqrdmlah (z1, z2, 11))
