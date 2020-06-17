/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bext_u16_tied1:
**	bext	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (bext_u16_tied1, svuint16_t,
		z0 = svbext_u16 (z0, z1),
		z0 = svbext (z0, z1))

/*
** bext_u16_tied2:
**	bext	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (bext_u16_tied2, svuint16_t,
		z0 = svbext_u16 (z1, z0),
		z0 = svbext (z1, z0))

/*
** bext_u16_untied:
**	bext	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (bext_u16_untied, svuint16_t,
		z0 = svbext_u16 (z1, z2),
		z0 = svbext (z1, z2))

/*
** bext_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	bext	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svbext_n_u16 (z0, x0),
		 z0 = svbext (z0, x0))

/*
** bext_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	bext	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bext_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svbext_n_u16 (z1, x0),
		 z0 = svbext (z1, x0))

/*
** bext_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	bext	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u16_tied1, svuint16_t,
		z0 = svbext_n_u16 (z0, 11),
		z0 = svbext (z0, 11))

/*
** bext_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	bext	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bext_11_u16_untied, svuint16_t,
		z0 = svbext_n_u16 (z1, 11),
		z0 = svbext (z1, 11))
