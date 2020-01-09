/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlbt_s32_tied1:
**	saddlbt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svaddlbt_s32 (z0, z1),
		    z0_res = svaddlbt (z0, z1))

/*
** addlbt_s32_tied2:
**	saddlbt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svaddlbt_s32 (z1, z0),
		    z0_res = svaddlbt (z1, z0))

/*
** addlbt_s32_untied:
**	saddlbt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s32_untied, svint32_t, svint16_t,
		    z0_res = svaddlbt_s32 (z1, z2),
		    z0_res = svaddlbt (z1, z2))

/*
** addlbt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	saddlbt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svaddlbt_n_s32 (z0, x0),
		     z0_res = svaddlbt (z0, x0))

/*
** addlbt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	saddlbt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svaddlbt_n_s32 (z1, x0),
		     z0_res = svaddlbt (z1, x0))

/*
** addlbt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	saddlbt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svaddlbt_n_s32 (z0, 11),
		    z0_res = svaddlbt (z0, 11))

/*
** addlbt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	saddlbt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svaddlbt_n_s32 (z1, 11),
		    z0_res = svaddlbt (z1, 11))
