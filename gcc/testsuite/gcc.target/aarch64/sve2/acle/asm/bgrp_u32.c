/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bgrp_u32_tied1:
**	bgrp	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (bgrp_u32_tied1, svuint32_t,
		z0 = svbgrp_u32 (z0, z1),
		z0 = svbgrp (z0, z1))

/*
** bgrp_u32_tied2:
**	bgrp	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (bgrp_u32_tied2, svuint32_t,
		z0 = svbgrp_u32 (z1, z0),
		z0 = svbgrp (z1, z0))

/*
** bgrp_u32_untied:
**	bgrp	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (bgrp_u32_untied, svuint32_t,
		z0 = svbgrp_u32 (z1, z2),
		z0 = svbgrp (z1, z2))

/*
** bgrp_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	bgrp	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svbgrp_n_u32 (z0, x0),
		 z0 = svbgrp (z0, x0))

/*
** bgrp_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	bgrp	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svbgrp_n_u32 (z1, x0),
		 z0 = svbgrp (z1, x0))

/*
** bgrp_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	bgrp	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u32_tied1, svuint32_t,
		z0 = svbgrp_n_u32 (z0, 11),
		z0 = svbgrp (z0, 11))

/*
** bgrp_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	bgrp	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u32_untied, svuint32_t,
		z0 = svbgrp_n_u32 (z1, 11),
		z0 = svbgrp (z1, 11))
