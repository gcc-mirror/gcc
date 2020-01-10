/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnb_u16_tied1:
**	subhnb	z0\.b, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svsubhnb_u16 (z0, z1),
		    z0_res = svsubhnb (z0, z1))

/*
** subhnb_u16_tied2:
**	subhnb	z0\.b, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u16_tied2, svuint8_t, svuint16_t,
		    z0_res = svsubhnb_u16 (z1, z0),
		    z0_res = svsubhnb (z1, z0))

/*
** subhnb_u16_untied:
**	subhnb	z0\.b, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svsubhnb_u16 (z1, z2),
		    z0_res = svsubhnb (z1, z2))

/*
** subhnb_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	subhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_u16_tied1, svuint8_t, svuint16_t, uint16_t,
		     z0_res = svsubhnb_n_u16 (z0, x0),
		     z0_res = svsubhnb (z0, x0))

/*
** subhnb_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	subhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_u16_untied, svuint8_t, svuint16_t, uint16_t,
		     z0_res = svsubhnb_n_u16 (z1, x0),
		     z0_res = svsubhnb (z1, x0))

/*
** subhnb_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	subhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svsubhnb_n_u16 (z0, 11),
		    z0_res = svsubhnb (z0, 11))

/*
** subhnb_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	subhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svsubhnb_n_u16 (z1, 11),
		    z0_res = svsubhnb (z1, 11))
