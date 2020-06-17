/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwt_u64_tied1:
**	uaddwt	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwt_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svaddwt_u64 (z0, z4),
	     z0 = svaddwt (z0, z4))

/*
** addwt_u64_tied2:
**	uaddwt	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (addwt_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svaddwt_u64 (z4, z0),
		 z0_res = svaddwt (z4, z0))

/*
** addwt_u64_untied:
**	uaddwt	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwt_u64_untied, svuint64_t, svuint32_t,
	     z0 = svaddwt_u64 (z1, z4),
	     z0 = svaddwt (z1, z4))

/*
** addwt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	uaddwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_u64_tied1, svuint64_t, uint32_t,
		 z0 = svaddwt_n_u64 (z0, x0),
		 z0 = svaddwt (z0, x0))

/*
** addwt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	uaddwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_u64_untied, svuint64_t, uint32_t,
		 z0 = svaddwt_n_u64 (z1, x0),
		 z0 = svaddwt (z1, x0))

/*
** addwt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	uaddwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_u64_tied1, svuint64_t,
		z0 = svaddwt_n_u64 (z0, 11),
		z0 = svaddwt (z0, 11))

/*
** addwt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	uaddwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_u64_untied, svuint64_t,
		z0 = svaddwt_n_u64 (z1, 11),
		z0 = svaddwt (z1, 11))
