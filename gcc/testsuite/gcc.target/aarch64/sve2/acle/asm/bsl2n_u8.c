/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bsl2n_u8_tied1:
**	bsl2n	z0\.d, z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_u8_tied1, svuint8_t,
		z0 = svbsl2n_u8 (z0, z1, z2),
		z0 = svbsl2n (z0, z1, z2))

/*
** bsl2n_u8_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_u8_tied2, svuint8_t,
		z0 = svbsl2n_u8 (z1, z0, z2),
		z0 = svbsl2n (z1, z0, z2))

/*
** bsl2n_u8_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bsl2n_u8_tied3, svuint8_t,
		z0 = svbsl2n_u8 (z1, z2, z0),
		z0 = svbsl2n (z1, z2, z0))

/*
** bsl2n_u8_untied:
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_u8_untied, svuint8_t,
		z0 = svbsl2n_u8 (z1, z2, z3),
		z0 = svbsl2n (z1, z2, z3))

/*
** bsl2n_w0_u8_tied1:
**	mov	(z[0-9]+)\.b, w0
**	bsl2n	z0\.d, z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl2n_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svbsl2n_n_u8 (z0, z1, x0),
		 z0 = svbsl2n (z0, z1, x0))

/*
** bsl2n_w0_u8_tied2:
**	mov	(z[0-9]+)\.b, w0
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, \2, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl2n_w0_u8_tied2, svuint8_t, uint8_t,
		 z0 = svbsl2n_n_u8 (z1, z0, x0),
		 z0 = svbsl2n (z1, z0, x0))

/*
** bsl2n_w0_u8_untied:
**	mov	(z[0-9]+)\.b, w0
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bsl2n_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svbsl2n_n_u8 (z1, z2, x0),
		 z0 = svbsl2n (z1, z2, x0))

/*
** bsl2n_11_u8_tied1:
**	mov	(z[0-9]+)\.b, #11
**	bsl2n	z0\.d, z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_11_u8_tied1, svuint8_t,
		z0 = svbsl2n_n_u8 (z0, z1, 11),
		z0 = svbsl2n (z0, z1, 11))

/*
** bsl2n_11_u8_tied2:
**	mov	(z[0-9]+)\.b, #11
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_11_u8_tied2, svuint8_t,
		z0 = svbsl2n_n_u8 (z1, z0, 11),
		z0 = svbsl2n (z1, z0, 11))

/*
** bsl2n_11_u8_untied:
**	mov	(z[0-9]+)\.b, #11
**	movprfx	z0, z1
**	bsl2n	z0\.d, z0\.d, z2\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (bsl2n_11_u8_untied, svuint8_t,
		z0 = svbsl2n_n_u8 (z1, z2, 11),
		z0 = svbsl2n (z1, z2, 11))
