/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmul_u8_tied1:
**	pmul	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (pmul_u8_tied1, svuint8_t,
		z0 = svpmul_u8 (z0, z1),
		z0 = svpmul (z0, z1))

/*
** pmul_u8_tied2:
**	pmul	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (pmul_u8_tied2, svuint8_t,
		z0 = svpmul_u8 (z1, z0),
		z0 = svpmul (z1, z0))

/*
** pmul_u8_untied:
**	pmul	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (pmul_u8_untied, svuint8_t,
		z0 = svpmul_u8 (z1, z2),
		z0 = svpmul (z1, z2))

/*
** pmul_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	pmul	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (pmul_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svpmul_n_u8 (z0, x0),
		 z0 = svpmul (z0, x0))

/*
** pmul_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	pmul	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (pmul_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svpmul_n_u8 (z1, x0),
		 z0 = svpmul (z1, x0))

/*
** pmul_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	pmul	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (pmul_11_u8_tied1, svuint8_t,
		z0 = svpmul_n_u8 (z0, 11),
		z0 = svpmul (z0, 11))

/*
** pmul_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	pmul	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (pmul_11_u8_untied, svuint8_t,
		z0 = svpmul_n_u8 (z1, 11),
		z0 = svpmul (z1, 11))
