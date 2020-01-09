/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwt_s32_tied1:
**	saddwt	z0\.s, z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwt_s32_tied1, svint32_t, svint16_t,
	     z0 = svaddwt_s32 (z0, z4),
	     z0 = svaddwt (z0, z4))

/*
** addwt_s32_tied2:
**	saddwt	z0\.s, z4\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (addwt_s32_tied2, svint32_t, svint16_t,
		 z0_res = svaddwt_s32 (z4, z0),
		 z0_res = svaddwt (z4, z0))

/*
** addwt_s32_untied:
**	saddwt	z0\.s, z1\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (addwt_s32_untied, svint32_t, svint16_t,
	     z0 = svaddwt_s32 (z1, z4),
	     z0 = svaddwt (z1, z4))

/*
** addwt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	saddwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s32_tied1, svint32_t, int16_t,
		 z0 = svaddwt_n_s32 (z0, x0),
		 z0 = svaddwt (z0, x0))

/*
** addwt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	saddwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s32_untied, svint32_t, int16_t,
		 z0 = svaddwt_n_s32 (z1, x0),
		 z0 = svaddwt (z1, x0))

/*
** addwt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	saddwt	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s32_tied1, svint32_t,
		z0 = svaddwt_n_s32 (z0, 11),
		z0 = svaddwt (z0, 11))

/*
** addwt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	saddwt	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s32_untied, svint32_t,
		z0 = svaddwt_n_s32 (z1, 11),
		z0 = svaddwt (z1, 11))
