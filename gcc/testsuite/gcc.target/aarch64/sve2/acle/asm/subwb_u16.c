/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subwb_u16_tied1:
**	usubwb	z0\.h, z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwb_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svsubwb_u16 (z0, z4),
	     z0 = svsubwb (z0, z4))

/*
** subwb_u16_tied2:
**	usubwb	z0\.h, z4\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (subwb_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svsubwb_u16 (z4, z0),
		 z0_res = svsubwb (z4, z0))

/*
** subwb_u16_untied:
**	usubwb	z0\.h, z1\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (subwb_u16_untied, svuint16_t, svuint8_t,
	     z0 = svsubwb_u16 (z1, z4),
	     z0 = svsubwb (z1, z4))

/*
** subwb_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	usubwb	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u16_tied1, svuint16_t, uint8_t,
		 z0 = svsubwb_n_u16 (z0, x0),
		 z0 = svsubwb (z0, x0))

/*
** subwb_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	usubwb	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subwb_w0_u16_untied, svuint16_t, uint8_t,
		 z0 = svsubwb_n_u16 (z1, x0),
		 z0 = svsubwb (z1, x0))

/*
** subwb_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	usubwb	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u16_tied1, svuint16_t,
		z0 = svsubwb_n_u16 (z0, 11),
		z0 = svsubwb (z0, 11))

/*
** subwb_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	usubwb	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subwb_11_u16_untied, svuint16_t,
		z0 = svsubwb_n_u16 (z1, 11),
		z0 = svsubwb (z1, 11))
