/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlbt_s16_tied1:
**	saddlbt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlbt_s16 (z0, z1),
		    z0_res = svaddlbt (z0, z1))

/*
** addlbt_s16_tied2:
**	saddlbt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svaddlbt_s16 (z1, z0),
		    z0_res = svaddlbt (z1, z0))

/*
** addlbt_s16_untied:
**	saddlbt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlbt_s16 (z1, z2),
		    z0_res = svaddlbt (z1, z2))

/*
** addlbt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	saddlbt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlbt_n_s16 (z0, x0),
		     z0_res = svaddlbt (z0, x0))

/*
** addlbt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	saddlbt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlbt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlbt_n_s16 (z1, x0),
		     z0_res = svaddlbt (z1, x0))

/*
** addlbt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	saddlbt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlbt_n_s16 (z0, 11),
		    z0_res = svaddlbt (z0, 11))

/*
** addlbt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	saddlbt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlbt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlbt_n_s16 (z1, 11),
		    z0_res = svaddlbt (z1, 11))
