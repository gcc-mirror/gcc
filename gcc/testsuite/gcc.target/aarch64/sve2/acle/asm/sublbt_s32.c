/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sublbt_s32_tied1:
**	ssublbt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublbt_s32 (z0, z1),
		    z0_res = svsublbt (z0, z1))

/*
** sublbt_s32_tied2:
**	ssublbt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svsublbt_s32 (z1, z0),
		    z0_res = svsublbt (z1, z0))

/*
** sublbt_s32_untied:
**	ssublbt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublbt_s32 (z1, z2),
		    z0_res = svsublbt (z1, z2))

/*
** sublbt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	ssublbt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svsublbt_n_s32 (z0, x0),
		     z0_res = svsublbt (z0, x0))

/*
** sublbt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	ssublbt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (sublbt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svsublbt_n_s32 (z1, x0),
		     z0_res = svsublbt (z1, x0))

/*
** sublbt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	ssublbt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svsublbt_n_s32 (z0, 11),
		    z0_res = svsublbt (z0, 11))

/*
** sublbt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	ssublbt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (sublbt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svsublbt_n_s32 (z1, 11),
		    z0_res = svsublbt (z1, 11))
