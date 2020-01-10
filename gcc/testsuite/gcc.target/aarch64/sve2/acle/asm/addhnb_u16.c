/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addhnb_u16_tied1:
**	addhnb	z0\.b, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svaddhnb_u16 (z0, z1),
		    z0_res = svaddhnb (z0, z1))

/*
** addhnb_u16_tied2:
**	addhnb	z0\.b, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_u16_tied2, svuint8_t, svuint16_t,
		    z0_res = svaddhnb_u16 (z1, z0),
		    z0_res = svaddhnb (z1, z0))

/*
** addhnb_u16_untied:
**	addhnb	z0\.b, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svaddhnb_u16 (z1, z2),
		    z0_res = svaddhnb (z1, z2))

/*
** addhnb_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	addhnb	z0\.b, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_w0_u16_tied1, svuint8_t, svuint16_t, uint16_t,
		     z0_res = svaddhnb_n_u16 (z0, x0),
		     z0_res = svaddhnb (z0, x0))

/*
** addhnb_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
**	addhnb	z0\.b, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_w0_u16_untied, svuint8_t, svuint16_t, uint16_t,
		     z0_res = svaddhnb_n_u16 (z1, x0),
		     z0_res = svaddhnb (z1, x0))

/*
** addhnb_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	addhnb	z0\.b, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svaddhnb_n_u16 (z0, 11),
		    z0_res = svaddhnb (z0, 11))

/*
** addhnb_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
**	addhnb	z0\.b, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svaddhnb_n_u16 (z1, 11),
		    z0_res = svaddhnb (z1, 11))
