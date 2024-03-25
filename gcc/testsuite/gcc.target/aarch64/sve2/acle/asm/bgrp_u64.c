/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2-bitperm"

/*
** bgrp_u64_tied1:
**	bgrp	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bgrp_u64_tied1, svuint64_t,
		z0 = svbgrp_u64 (z0, z1),
		z0 = svbgrp (z0, z1))

/*
** bgrp_u64_tied2:
**	bgrp	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bgrp_u64_tied2, svuint64_t,
		z0 = svbgrp_u64 (z1, z0),
		z0 = svbgrp (z1, z0))

/*
** bgrp_u64_untied:
**	bgrp	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bgrp_u64_untied, svuint64_t,
		z0 = svbgrp_u64 (z1, z2),
		z0 = svbgrp (z1, z2))

/*
** bgrp_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	bgrp	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svbgrp_n_u64 (z0, x0),
		 z0 = svbgrp (z0, x0))

/*
** bgrp_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	bgrp	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bgrp_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svbgrp_n_u64 (z1, x0),
		 z0 = svbgrp (z1, x0))

/*
** bgrp_11_u64_tied1:
**	mov	(z[0-9]+\.d), #11
**	bgrp	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u64_tied1, svuint64_t,
		z0 = svbgrp_n_u64 (z0, 11),
		z0 = svbgrp (z0, 11))

/*
** bgrp_11_u64_untied:
**	mov	(z[0-9]+\.d), #11
**	bgrp	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bgrp_11_u64_untied, svuint64_t,
		z0 = svbgrp_n_u64 (z1, 11),
		z0 = svbgrp (z1, 11))
