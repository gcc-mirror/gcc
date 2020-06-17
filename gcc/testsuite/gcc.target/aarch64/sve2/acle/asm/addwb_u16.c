/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addwb_u16_tied1:
**	uaddwb	z0\.h, z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (addwb_u16_tied1, svuint16_t, svuint8_t,
	     z0 = svaddwb_u16 (z0, z4),
	     z0 = svaddwb (z0, z4))

/*
** addwb_u16_tied2:
**	uaddwb	z0\.h, z4\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (addwb_u16_tied2, svuint16_t, svuint8_t,
		 z0_res = svaddwb_u16 (z4, z0),
		 z0_res = svaddwb (z4, z0))

/*
** addwb_u16_untied:
**	uaddwb	z0\.h, z1\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (addwb_u16_untied, svuint16_t, svuint8_t,
	     z0 = svaddwb_u16 (z1, z4),
	     z0 = svaddwb (z1, z4))

/*
** addwb_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	uaddwb	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u16_tied1, svuint16_t, uint8_t,
		 z0 = svaddwb_n_u16 (z0, x0),
		 z0 = svaddwb (z0, x0))

/*
** addwb_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	uaddwb	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (addwb_w0_u16_untied, svuint16_t, uint8_t,
		 z0 = svaddwb_n_u16 (z1, x0),
		 z0 = svaddwb (z1, x0))

/*
** addwb_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	uaddwb	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u16_tied1, svuint16_t,
		z0 = svaddwb_n_u16 (z0, 11),
		z0 = svaddwb (z0, 11))

/*
** addwb_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	uaddwb	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (addwb_11_u16_untied, svuint16_t,
		z0 = svaddwb_n_u16 (z1, 11),
		z0 = svaddwb (z1, 11))
