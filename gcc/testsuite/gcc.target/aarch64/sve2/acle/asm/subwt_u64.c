/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_u64_tied1:
**	usubwt	z0\.d, z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwt_u64_tied1, svuint64_t, svuint32_t,
	     z0 = svsubwt_u64 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_u64_tied2:
**	usubwt	z0\.d, z4\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (subwt_u64_tied2, svuint64_t, svuint32_t,
		 z0_res = svsubwt_u64 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_u64_untied:
**	usubwt	z0\.d, z1\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (subwt_u64_untied, svuint64_t, svuint32_t,
	     z0 = svsubwt_u64 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_u64_tied1:
**	mov	(z[0-9]+\.s), w0
**	usubwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u64_tied1, svuint64_t, uint32_t,
		 z0 = svsubwt_n_u64 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_u64_untied:
**	mov	(z[0-9]+\.s), w0
**	usubwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u64_untied, svuint64_t, uint32_t,
		 z0 = svsubwt_n_u64 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_u64_tied1:
**	mov	(z[0-9]+\.s), #11
**	usubwt	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u64_tied1, svuint64_t,
		z0 = svsubwt_n_u64 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_u64_untied:
**	mov	(z[0-9]+\.s), #11
**	usubwt	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u64_untied, svuint64_t,
		z0 = svsubwt_n_u64 (z1, 11),
		z0 = svsubwt (z1, 11))
