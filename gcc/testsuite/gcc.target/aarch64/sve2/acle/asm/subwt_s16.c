/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_s16_tied1:
**	ssubwt	z0\.h, z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwt_s16_tied1, svint16_t, svint8_t,
	     z0 = svsubwt_s16 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_s16_tied2:
**	ssubwt	z0\.h, z4\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (subwt_s16_tied2, svint16_t, svint8_t,
		 z0_res = svsubwt_s16 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_s16_untied:
**	ssubwt	z0\.h, z1\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwt_s16_untied, svint16_t, svint8_t,
	     z0 = svsubwt_s16 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	ssubwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s16_tied1, svint16_t, int8_t,
		 z0 = svsubwt_n_s16 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	ssubwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_s16_untied, svint16_t, int8_t,
		 z0 = svsubwt_n_s16 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	ssubwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s16_tied1, svint16_t,
		z0 = svsubwt_n_s16 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	ssubwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_s16_untied, svint16_t,
		z0 = svsubwt_n_s16 (z1, 11),
		z0 = svsubwt (z1, 11))
