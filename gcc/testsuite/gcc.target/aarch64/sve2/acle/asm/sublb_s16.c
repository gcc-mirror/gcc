/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublb_s16_tied1:
**	ssublb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublb_s16 (z0, z1),
		    z0_res = svsublb (z0, z1))

/*
** sublb_s16_tied2:
**	ssublb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s16_tied2, svint16_t, svint8_t,
		    z0_res = svsublb_s16 (z1, z0),
		    z0_res = svsublb (z1, z0))

/*
** sublb_s16_untied:
**	ssublb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublb_s16 (z1, z2),
		    z0_res = svsublb (z1, z2))

/*
** sublb_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	ssublb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svsublb_n_s16 (z0, x0),
		     z0_res = svsublb (z0, x0))

/*
** sublb_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	ssublb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublb_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svsublb_n_s16 (z1, x0),
		     z0_res = svsublb (z1, x0))

/*
** sublb_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	ssublb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublb_n_s16 (z0, 11),
		    z0_res = svsublb (z0, 11))

/*
** sublb_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	ssublb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublb_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublb_n_s16 (z1, 11),
		    z0_res = svsublb (z1, 11))
