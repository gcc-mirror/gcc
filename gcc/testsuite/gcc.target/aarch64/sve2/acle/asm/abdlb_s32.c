/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlb_s32_tied1:
**	sabdlb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s32_tied1, svint32_t, svint16_t,
		    z0_res = svabdlb_s32 (z0, z1),
		    z0_res = svabdlb (z0, z1))

/*
** abdlb_s32_tied2:
**	sabdlb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s32_tied2, svint32_t, svint16_t,
		    z0_res = svabdlb_s32 (z1, z0),
		    z0_res = svabdlb (z1, z0))

/*
** abdlb_s32_untied:
**	sabdlb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_s32_untied, svint32_t, svint16_t,
		    z0_res = svabdlb_s32 (z1, z2),
		    z0_res = svabdlb (z1, z2))

/*
** abdlb_w0_s32_tied1:
**	mov	(z[0-9]+\.h), w0
**	sabdlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_s32_tied1, svint32_t, svint16_t, int16_t,
		     z0_res = svabdlb_n_s32 (z0, x0),
		     z0_res = svabdlb (z0, x0))

/*
** abdlb_w0_s32_untied:
**	mov	(z[0-9]+\.h), w0
**	sabdlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_s32_untied, svint32_t, svint16_t, int16_t,
		     z0_res = svabdlb_n_s32 (z1, x0),
		     z0_res = svabdlb (z1, x0))

/*
** abdlb_11_s32_tied1:
**	mov	(z[0-9]+\.h), #11
**	sabdlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_s32_tied1, svint32_t, svint16_t,
		    z0_res = svabdlb_n_s32 (z0, 11),
		    z0_res = svabdlb (z0, 11))

/*
** abdlb_11_s32_untied:
**	mov	(z[0-9]+\.h), #11
**	sabdlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_s32_untied, svint32_t, svint16_t,
		    z0_res = svabdlb_n_s32 (z1, 11),
		    z0_res = svabdlb (z1, 11))
