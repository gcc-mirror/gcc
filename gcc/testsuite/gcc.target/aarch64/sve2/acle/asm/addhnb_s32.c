/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addhnb_s32_tied1:
**	addhnb	z0\.h, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s32_tied1, svint16_t, svint32_t,
		    z0_res = svaddhnb_s32 (z0, z1),
		    z0_res = svaddhnb (z0, z1))

/*
** addhnb_s32_tied2:
**	addhnb	z0\.h, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s32_tied2, svint16_t, svint32_t,
		    z0_res = svaddhnb_s32 (z1, z0),
		    z0_res = svaddhnb (z1, z0))

/*
** addhnb_s32_untied:
**	addhnb	z0\.h, (z1\.s, z2\.s|z2\.s, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_s32_untied, svint16_t, svint32_t,
		    z0_res = svaddhnb_s32 (z1, z2),
		    z0_res = svaddhnb (z1, z2))

/*
** addhnb_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	addhnb	z0\.h, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_w0_s32_tied1, svint16_t, svint32_t, int32_t,
		     z0_res = svaddhnb_n_s32 (z0, x0),
		     z0_res = svaddhnb (z0, x0))

/*
** addhnb_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
**	addhnb	z0\.h, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_ZX (addhnb_w0_s32_untied, svint16_t, svint32_t, int32_t,
		     z0_res = svaddhnb_n_s32 (z1, x0),
		     z0_res = svaddhnb (z1, x0))

/*
** addhnb_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	addhnb	z0\.h, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_s32_tied1, svint16_t, svint32_t,
		    z0_res = svaddhnb_n_s32 (z0, 11),
		    z0_res = svaddhnb (z0, 11))

/*
** addhnb_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
**	addhnb	z0\.h, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (addhnb_11_s32_untied, svint16_t, svint32_t,
		    z0_res = svaddhnb_n_s32 (z1, 11),
		    z0_res = svaddhnb (z1, 11))
