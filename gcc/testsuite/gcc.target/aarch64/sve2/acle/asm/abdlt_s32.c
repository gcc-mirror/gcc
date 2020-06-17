/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlt_s32_tied1:
**	sabdlt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s32_tied1, svint32_t, svint16_t,
		    z0_res = svabdlt_s32 (z0, z1),
		    z0_res = svabdlt (z0, z1))

/*
** abdlt_s32_tied2:
**	sabdlt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s32_tied2, svint32_t, svint16_t,
		    z0_res = svabdlt_s32 (z1, z0),
		    z0_res = svabdlt (z1, z0))

/*
** abdlt_s32_untied:
**	sabdlt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s32_untied, svint32_t, svint16_t,
		    z0_res = svabdlt_s32 (z1, z2),
		    z0_res = svabdlt (z1, z2))

/*
** abdlt_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sabdlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svabdlt_n_s32 (z0, x0),
		     z0_res = svabdlt (z0, x0))

/*
** abdlt_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	sabdlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svabdlt_n_s32 (z1, x0),
		     z0_res = svabdlt (z1, x0))

/*
** abdlt_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sabdlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svabdlt_n_s32 (z0, 11),
		    z0_res = svabdlt (z0, 11))

/*
** abdlt_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	sabdlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svabdlt_n_s32 (z1, 11),
		    z0_res = svabdlt (z1, 11))
