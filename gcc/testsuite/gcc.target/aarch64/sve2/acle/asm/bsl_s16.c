/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bsl_s16_tied1:
**	bsl	z0\.d, z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_s16_tied1, svint16_t,
		z0 = svbsl_s16 (z0, z1, z2),
		z0 = svbsl (z0, z1, z2))

/*
** bsl_s16_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_s16_tied2, svint16_t,
		z0 = svbsl_s16 (z1, z0, z2),
		z0 = svbsl (z1, z0, z2))

/*
** bsl_s16_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bsl_s16_tied3, svint16_t,
		z0 = svbsl_s16 (z1, z2, z0),
		z0 = svbsl (z1, z2, z0))

/*
** bsl_s16_untied:
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_s16_untied, svint16_t,
		z0 = svbsl_s16 (z1, z2, z3),
		z0 = svbsl (z1, z2, z3))

/*
** bsl_w0_s16_tied1:
**	mov	(z[0-9]+)\.h, w0
**	bsl	z0\.d, z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svbsl_n_s16 (z0, z1, x0),
		 z0 = svbsl (z0, z1, x0))

/*
** bsl_w0_s16_tied2:
**	mov	(z[0-9]+)\.h, w0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, \2, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl_w0_s16_tied2, svint16_t, int16_t,
		 z0 = svbsl_n_s16 (z1, z0, x0),
		 z0 = svbsl (z1, z0, x0))

/*
** bsl_w0_s16_untied:
**	mov	(z[0-9]+)\.h, w0
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl_w0_s16_untied, svint16_t, int16_t,
		 z0 = svbsl_n_s16 (z1, z2, x0),
		 z0 = svbsl (z1, z2, x0))

/*
** bsl_11_s16_tied1:
**	mov	(z[0-9]+)\.h, #11
**	bsl	z0\.d, z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_11_s16_tied1, svint16_t,
		z0 = svbsl_n_s16 (z0, z1, 11),
		z0 = svbsl (z0, z1, 11))

/*
** bsl_11_s16_tied2:
**	mov	(z[0-9]+)\.h, #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_11_s16_tied2, svint16_t,
		z0 = svbsl_n_s16 (z1, z0, 11),
		z0 = svbsl (z1, z0, 11))

/*
** bsl_11_s16_untied:
**	mov	(z[0-9]+)\.h, #11
**	movprfx	z0, z1
**	bsl	z0\.d, z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl_11_s16_untied, svint16_t,
		z0 = svbsl_n_s16 (z1, z2, 11),
		z0 = svbsl (z1, z2, 11))
