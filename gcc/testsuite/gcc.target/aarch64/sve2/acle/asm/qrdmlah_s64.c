/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrdmlah_s64_tied1:
**	sqrdmlah	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s64_tied1, svint64_t,
		z0 = svqrdmlah_s64 (z0, z1, z2),
		z0 = svqrdmlah (z0, z1, z2))

/*
** qrdmlah_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s64_tied2, svint64_t,
		z0 = svqrdmlah_s64 (z1, z0, z2),
		z0 = svqrdmlah (z1, z0, z2))

/*
** qrdmlah_s64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s64_tied3, svint64_t,
		z0 = svqrdmlah_s64 (z1, z2, z0),
		z0 = svqrdmlah (z1, z2, z0))

/*
** qrdmlah_s64_untied:
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_s64_untied, svint64_t,
		z0 = svqrdmlah_s64 (z1, z2, z3),
		z0 = svqrdmlah (z1, z2, z3))

/*
** qrdmlah_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	sqrdmlah	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_x0_s64_tied1, svint64_t, int64_t,
		 z0 = svqrdmlah_n_s64 (z0, z1, x0),
		 z0 = svqrdmlah (z0, z1, x0))

/*
** qrdmlah_x0_s64_tied2:
**	mov	(z[0-9]+\.d), x0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_x0_s64_tied2, svint64_t, int64_t,
		 z0 = svqrdmlah_n_s64 (z1, z0, x0),
		 z0 = svqrdmlah (z1, z0, x0))

/*
** qrdmlah_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (qrdmlah_x0_s64_untied, svint64_t, int64_t,
		 z0 = svqrdmlah_n_s64 (z1, z2, x0),
		 z0 = svqrdmlah (z1, z2, x0))

/*
** qrdmlah_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	sqrdmlah	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s64_tied1, svint64_t,
		z0 = svqrdmlah_n_s64 (z0, z1, 11),
		z0 = svqrdmlah (z0, z1, 11))

/*
** qrdmlah_11_s64_tied2:
**	mov	(z[0-9]+\.d), #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, \2, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s64_tied2, svint64_t,
		z0 = svqrdmlah_n_s64 (z1, z0, 11),
		z0 = svqrdmlah (z1, z0, 11))

/*
** qrdmlah_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	movprfx	z0, z1
**	sqrdmlah	z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (qrdmlah_11_s64_untied, svint64_t,
		z0 = svqrdmlah_n_s64 (z1, z2, 11),
		z0 = svqrdmlah (z1, z2, 11))
