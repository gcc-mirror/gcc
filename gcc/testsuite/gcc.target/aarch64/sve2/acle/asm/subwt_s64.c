/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_s64_tied1:
**	ssubwt	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwt_s64_tied1, svint64_t, svint32_t,
	     z0 = svsubwt_s64 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_s64_tied2:
**	ssubwt	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (subwt_s64_tied2, svint64_t, svint32_t,
		 z0_res = svsubwt_s64 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_s64_untied:
**	ssubwt	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwt_s64_untied, svint64_t, svint32_t,
	     z0 = svsubwt_s64 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	ssubwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s64_tied1, svint64_t, int32_t,
		 z0 = svsubwt_n_s64 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	ssubwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s64_untied, svint64_t, int32_t,
		 z0 = svsubwt_n_s64 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	ssubwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s64_tied1, svint64_t,
		z0 = svsubwt_n_s64 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	ssubwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s64_untied, svint64_t,
		z0 = svsubwt_n_s64 (z1, 11),
		z0 = svsubwt (z1, 11))
