/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullt_pair_u8_tied1:
**	pmullt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u8_tied1, svuint8_t,
		z0 = svpmullt_pair_u8 (z0, z1),
		z0 = svpmullt_pair (z0, z1))

/*
** pmullt_pair_u8_tied2:
**	pmullt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u8_tied2, svuint8_t,
		z0 = svpmullt_pair_u8 (z1, z0),
		z0 = svpmullt_pair (z1, z0))

/*
** pmullt_pair_u8_untied:
**	pmullt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_u8_untied, svuint8_t,
		z0 = svpmullt_pair_u8 (z1, z2),
		z0 = svpmullt_pair (z1, z2))

/*
** pmullt_pair_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	pmullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svpmullt_pair_n_u8 (z0, x0),
		 z0 = svpmullt_pair (z0, x0))

/*
** pmullt_pair_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	pmullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (pmullt_pair_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svpmullt_pair_n_u8 (z1, x0),
		 z0 = svpmullt_pair (z1, x0))

/*
** pmullt_pair_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	pmullt	z0\.h, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u8_tied1, svuint8_t,
		z0 = svpmullt_pair_n_u8 (z0, 11),
		z0 = svpmullt_pair (z0, 11))

/*
** pmullt_pair_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	pmullt	z0\.h, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (pmullt_pair_11_u8_untied, svuint8_t,
		z0 = svpmullt_pair_n_u8 (z1, 11),
		z0 = svpmullt_pair (z1, 11))
