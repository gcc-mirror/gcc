/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwt_s16_tied1:
**	saddwt	z0\.h, z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (addwt_s16_tied1, svint16_t, svint8_t,
	     z0 = svaddwt_s16 (z0, z4),
	     z0 = svaddwt (z0, z4))

/*
** addwt_s16_tied2:
**	saddwt	z0\.h, z4\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (addwt_s16_tied2, svint16_t, svint8_t,
		 z0_res = svaddwt_s16 (z4, z0),
		 z0_res = svaddwt (z4, z0))

/*
** addwt_s16_untied:
**	saddwt	z0\.h, z1\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (addwt_s16_untied, svint16_t, svint8_t,
	     z0 = svaddwt_s16 (z1, z4),
	     z0 = svaddwt (z1, z4))

/*
** addwt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	saddwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s16_tied1, svint16_t, int8_t,
		 z0 = svaddwt_n_s16 (z0, x0),
		 z0 = svaddwt (z0, x0))

/*
** addwt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	saddwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (addwt_w0_s16_untied, svint16_t, int8_t,
		 z0 = svaddwt_n_s16 (z1, x0),
		 z0 = svaddwt (z1, x0))

/*
** addwt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	saddwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s16_tied1, svint16_t,
		z0 = svaddwt_n_s16 (z0, 11),
		z0 = svaddwt (z0, 11))

/*
** addwt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	saddwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (addwt_11_s16_untied, svint16_t,
		z0 = svaddwt_n_s16 (z1, 11),
		z0 = svaddwt (z1, 11))
