/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublb_s32_tied1:
**	ssublb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublb_s32 (z0, z1),
		    z0_res = svsublb (z0, z1))

/*
** sublb_s32_tied2:
**	ssublb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s32_tied2, svint32_t, svint16_t,
		    z0_res = svsublb_s32 (z1, z0),
		    z0_res = svsublb (z1, z0))

/*
** sublb_s32_untied:
**	ssublb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublb_s32 (z1, z2),
		    z0_res = svsublb (z1, z2))

/*
** sublb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssublb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svsublb_n_s32 (z0, x0),
		     z0_res = svsublb (z0, x0))

/*
** sublb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssublb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svsublb_n_s32 (z1, x0),
		     z0_res = svsublb (z1, x0))

/*
** sublb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssublb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublb_n_s32 (z0, 11),
		    z0_res = svsublb (z0, 11))

/*
** sublb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssublb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublb_n_s32 (z1, 11),
		    z0_res = svsublb (z1, 11))
