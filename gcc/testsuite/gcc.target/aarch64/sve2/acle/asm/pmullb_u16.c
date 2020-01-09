/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pmullb_u16_tied1:
**	pmullb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svpmullb_u16 (z0, z1),
		    z0_res = svpmullb (z0, z1))

/*
** pmullb_u16_tied2:
**	pmullb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u16_tied2, svuint16_t, svuint8_t,
		    z0_res = svpmullb_u16 (z1, z0),
		    z0_res = svpmullb (z1, z0))

/*
** pmullb_u16_untied:
**	pmullb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svpmullb_u16 (z1, z2),
		    z0_res = svpmullb (z1, z2))

/*
** pmullb_w0_u16_tied1:
**	mov	(z[0-9]+\.b), w0
**	pmullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullb_w0_u16_tied1, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svpmullb_n_u16 (z0, x0),
		     z0_res = svpmullb (z0, x0))

/*
** pmullb_w0_u16_untied:
**	mov	(z[0-9]+\.b), w0
**	pmullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (pmullb_w0_u16_untied, svuint16_t, svuint8_t, uint8_t,
		     z0_res = svpmullb_n_u16 (z1, x0),
		     z0_res = svpmullb (z1, x0))

/*
** pmullb_11_u16_tied1:
**	mov	(z[0-9]+\.b), #11
**	pmullb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_11_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svpmullb_n_u16 (z0, 11),
		    z0_res = svpmullb (z0, 11))

/*
** pmullb_11_u16_untied:
**	mov	(z[0-9]+\.b), #11
**	pmullb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (pmullb_11_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svpmullb_n_u16 (z1, 11),
		    z0_res = svpmullb (z1, 11))
