/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublb_u32_tied1:
**	usublb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svsublb_u32 (z0, z1),
		    z0_res = svsublb (z0, z1))

/*
** sublb_u32_tied2:
**	usublb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svsublb_u32 (z1, z0),
		    z0_res = svsublb (z1, z0))

/*
** sublb_u32_untied:
**	usublb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svsublb_u32 (z1, z2),
		    z0_res = svsublb (z1, z2))

/*
** sublb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	usublb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svsublb_n_u32 (z0, x0),
		     z0_res = svsublb (z0, x0))

/*
** sublb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	usublb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svsublb_n_u32 (z1, x0),
		     z0_res = svsublb (z1, x0))

/*
** sublb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	usublb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svsublb_n_u32 (z0, 11),
		    z0_res = svsublb (z0, 11))

/*
** sublb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	usublb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svsublb_n_u32 (z1, 11),
		    z0_res = svsublb (z1, 11))
