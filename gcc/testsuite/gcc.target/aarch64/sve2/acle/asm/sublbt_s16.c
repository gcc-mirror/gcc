/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublbt_s16_tied1:
**	ssublbt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublbt_s16 (z0, z1),
		    z0_res = svsublbt (z0, z1))

/*
** sublbt_s16_tied2:
**	ssublbt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svsublbt_s16 (z1, z0),
		    z0_res = svsublbt (z1, z0))

/*
** sublbt_s16_untied:
**	ssublbt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublbt_s16 (z1, z2),
		    z0_res = svsublbt (z1, z2))

/*
** sublbt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	ssublbt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svsublbt_n_s16 (z0, x0),
		     z0_res = svsublbt (z0, x0))

/*
** sublbt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	ssublbt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svsublbt_n_s16 (z1, x0),
		     z0_res = svsublbt (z1, x0))

/*
** sublbt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	ssublbt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svsublbt_n_s16 (z0, 11),
		    z0_res = svsublbt (z0, 11))

/*
** sublbt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	ssublbt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svsublbt_n_s16 (z1, 11),
		    z0_res = svsublbt (z1, 11))
