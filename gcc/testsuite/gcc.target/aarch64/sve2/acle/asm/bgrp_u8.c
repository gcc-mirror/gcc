/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bgrp_u8_tied1:
**	bgrp	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (bgrp_u8_tied1, svuint8_t,
		z0 = svbgrp_u8 (z0, z1),
		z0 = svbgrp (z0, z1))

/*
** bgrp_u8_tied2:
**	bgrp	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (bgrp_u8_tied2, svuint8_t,
		z0 = svbgrp_u8 (z1, z0),
		z0 = svbgrp (z1, z0))

/*
** bgrp_u8_untied:
**	bgrp	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (bgrp_u8_untied, svuint8_t,
		z0 = svbgrp_u8 (z1, z2),
		z0 = svbgrp (z1, z2))

/*
** bgrp_w0_u8_tied1:
**	mov	(z[0-9]+\.b), w0
**	bgrp	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u8_tied1, svuint8_t, uint8_t,
		 z0 = svbgrp_n_u8 (z0, x0),
		 z0 = svbgrp (z0, x0))

/*
** bgrp_w0_u8_untied:
**	mov	(z[0-9]+\.b), w0
**	bgrp	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u8_untied, svuint8_t, uint8_t,
		 z0 = svbgrp_n_u8 (z1, x0),
		 z0 = svbgrp (z1, x0))

/*
** bgrp_11_u8_tied1:
**	mov	(z[0-9]+\.b), #11
**	bgrp	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u8_tied1, svuint8_t,
		z0 = svbgrp_n_u8 (z0, 11),
		z0 = svbgrp (z0, 11))

/*
** bgrp_11_u8_untied:
**	mov	(z[0-9]+\.b), #11
**	bgrp	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u8_untied, svuint8_t,
		z0 = svbgrp_n_u8 (z1, 11),
		z0 = svbgrp (z1, 11))
