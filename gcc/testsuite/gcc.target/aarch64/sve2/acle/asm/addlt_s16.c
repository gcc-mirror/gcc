/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlt_s16_tied1:
**	saddlt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlt_s16 (z0, z1),
		    z0_res = svaddlt (z0, z1))

/*
** addlt_s16_tied2:
**	saddlt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svaddlt_s16 (z1, z0),
		    z0_res = svaddlt (z1, z0))

/*
** addlt_s16_untied:
**	saddlt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlt_s16 (z1, z2),
		    z0_res = svaddlt (z1, z2))

/*
** addlt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	saddlt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlt_n_s16 (z0, x0),
		     z0_res = svaddlt (z0, x0))

/*
** addlt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	saddlt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlt_n_s16 (z1, x0),
		     z0_res = svaddlt (z1, x0))

/*
** addlt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	saddlt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlt_n_s16 (z0, 11),
		    z0_res = svaddlt (z0, 11))

/*
** addlt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	saddlt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlt_n_s16 (z1, 11),
		    z0_res = svaddlt (z1, 11))
