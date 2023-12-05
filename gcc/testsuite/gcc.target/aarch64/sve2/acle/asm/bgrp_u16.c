/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bgrp_u16_tied1:
**	bgrp	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (bgrp_u16_tied1, svuint16_t,
		z0 = svbgrp_u16 (z0, z1),
		z0 = svbgrp (z0, z1))

/*
** bgrp_u16_tied2:
**	bgrp	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (bgrp_u16_tied2, svuint16_t,
		z0 = svbgrp_u16 (z1, z0),
		z0 = svbgrp (z1, z0))

/*
** bgrp_u16_untied:
**	bgrp	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (bgrp_u16_untied, svuint16_t,
		z0 = svbgrp_u16 (z1, z2),
		z0 = svbgrp (z1, z2))

/*
** bgrp_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	bgrp	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u16_tied1, svuint16_t, uint16_t,
		 z0 = svbgrp_n_u16 (z0, x0),
		 z0 = svbgrp (z0, x0))

/*
** bgrp_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	bgrp	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u16_untied, svuint16_t, uint16_t,
		 z0 = svbgrp_n_u16 (z1, x0),
		 z0 = svbgrp (z1, x0))

/*
** bgrp_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	bgrp	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u16_tied1, svuint16_t,
		z0 = svbgrp_n_u16 (z0, 11),
		z0 = svbgrp (z0, 11))

/*
** bgrp_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	bgrp	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u16_untied, svuint16_t,
		z0 = svbgrp_n_u16 (z1, 11),
		z0 = svbgrp (z1, 11))
