/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_s32_tied1:
**	ssubwt	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwt_s32_tied1, svint32_t, svint16_t,
	     z0 = svsubwt_s32 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_s32_tied2:
**	ssubwt	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (subwt_s32_tied2, svint32_t, svint16_t,
		 z0_res = svsubwt_s32 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_s32_untied:
**	ssubwt	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (subwt_s32_untied, svint32_t, svint16_t,
	     z0 = svsubwt_s32 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssubwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s32_tied1, svint32_t, int16_t,
		 z0 = svsubwt_n_s32 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssubwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s32_untied, svint32_t, int16_t,
		 z0 = svsubwt_n_s32 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssubwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s32_tied1, svint32_t,
		z0 = svsubwt_n_s32 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssubwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s32_untied, svint32_t,
		z0 = svsubwt_n_s32 (z1, 11),
		z0 = svsubwt (z1, 11))
