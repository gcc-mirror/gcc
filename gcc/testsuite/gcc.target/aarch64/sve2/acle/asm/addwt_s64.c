/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwt_s64_tied1:
**	saddwt	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwt_s64_tied1, svint64_t, svint32_t,
	     z0 = svaddwt_s64 (z0, z4),
	     z0 = svaddwt (z0, z4))

/*
** addwt_s64_tied2:
**	saddwt	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (addwt_s64_tied2, svint64_t, svint32_t,
		 z0_res = svaddwt_s64 (z4, z0),
		 z0_res = svaddwt (z4, z0))

/*
** addwt_s64_untied:
**	saddwt	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (addwt_s64_untied, svint64_t, svint32_t,
	     z0 = svaddwt_s64 (z1, z4),
	     z0 = svaddwt (z1, z4))

/*
** addwt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	saddwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s64_tied1, svint64_t, int32_t,
		 z0 = svaddwt_n_s64 (z0, x0),
		 z0 = svaddwt (z0, x0))

/*
** addwt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	saddwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s64_untied, svint64_t, int32_t,
		 z0 = svaddwt_n_s64 (z1, x0),
		 z0 = svaddwt (z1, x0))

/*
** addwt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	saddwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s64_tied1, svint64_t,
		z0 = svaddwt_n_s64 (z0, 11),
		z0 = svaddwt (z0, 11))

/*
** addwt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	saddwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s64_untied, svint64_t,
		z0 = svaddwt_n_s64 (z1, 11),
		z0 = svaddwt (z1, 11))
