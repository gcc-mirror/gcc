/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwt_u16_tied1:
**	usubwt	z0\.h, z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwt_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svsubwt_u16 (z0, z4),
	     z0 = svsubwt (z0, z4))

/*
** subwt_u16_tied2:
**	usubwt	z0\.h, z4\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (subwt_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svsubwt_u16 (z4, z0),
		 z0_res = svsubwt (z4, z0))

/*
** subwt_u16_untied:
**	usubwt	z0\.h, z1\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwt_u16_untied, svuint16_t, svuint8_t,
	     z0 = svsubwt_u16 (z1, z4),
	     z0 = svsubwt (z1, z4))

/*
** subwt_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	usubwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u16_tied1, svuint16_t, uint8_t,
		 z0 = svsubwt_n_u16 (z0, x0),
		 z0 = svsubwt (z0, x0))

/*
** subwt_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	usubwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwt_w0_u16_untied, svuint16_t, uint8_t,
		 z0 = svsubwt_n_u16 (z1, x0),
		 z0 = svsubwt (z1, x0))

/*
** subwt_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	usubwt	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u16_tied1, svuint16_t,
		z0 = svsubwt_n_u16 (z0, 11),
		z0 = svsubwt (z0, 11))

/*
** subwt_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	usubwt	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwt_11_u16_untied, svuint16_t,
		z0 = svsubwt_n_u16 (z1, 11),
		z0 = svsubwt (z1, 11))
