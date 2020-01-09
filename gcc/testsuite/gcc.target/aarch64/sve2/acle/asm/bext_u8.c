/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bext_u8_tied1:
**	bext	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (bext_u8_tied1, svuint8_t,
		z0 = svbext_u8 (z0, z1),
		z0 = svbext (z0, z1))

/*
** bext_u8_tied2:
**	bext	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (bext_u8_tied2, svuint8_t,
		z0 = svbext_u8 (z1, z0),
		z0 = svbext (z1, z0))

/*
** bext_u8_untied:
**	bext	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (bext_u8_untied, svuint8_t,
		z0 = svbext_u8 (z1, z2),
		z0 = svbext (z1, z2))

/*
** bext_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	bext	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svbext_n_u8 (z0, x0),
		 z0 = svbext (z0, x0))

/*
** bext_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	bext	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svbext_n_u8 (z1, x0),
		 z0 = svbext (z1, x0))

/*
** bext_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	bext	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u8_tied1, svuint8_t,
		z0 = svbext_n_u8 (z0, 11),
		z0 = svbext (z0, 11))

/*
** bext_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	bext	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u8_untied, svuint8_t,
		z0 = svbext_n_u8 (z1, 11),
		z0 = svbext (z1, 11))
