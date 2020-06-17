/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlt_s32_tied1:
**	saddlt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svaddlt_s32 (z0, z1),
		    z0_res = svaddlt (z0, z1))

/*
** addlt_s32_tied2:
**	saddlt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svaddlt_s32 (z1, z0),
		    z0_res = svaddlt (z1, z0))

/*
** addlt_s32_untied:
**	saddlt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_s32_untied, svint32_t, svint16_t,
		    z0_res = svaddlt_s32 (z1, z2),
		    z0_res = svaddlt (z1, z2))

/*
** addlt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	saddlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svaddlt_n_s32 (z0, x0),
		     z0_res = svaddlt (z0, x0))

/*
** addlt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	saddlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svaddlt_n_s32 (z1, x0),
		     z0_res = svaddlt (z1, x0))

/*
** addlt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	saddlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svaddlt_n_s32 (z0, 11),
		    z0_res = svaddlt (z0, 11))

/*
** addlt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	saddlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svaddlt_n_s32 (z1, 11),
		    z0_res = svaddlt (z1, 11))
