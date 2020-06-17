/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlb_u32_tied1:
**	uabdlb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svabdlb_u32 (z0, z1),
		    z0_res = svabdlb (z0, z1))

/*
** abdlb_u32_tied2:
**	uabdlb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svabdlb_u32 (z1, z0),
		    z0_res = svabdlb (z1, z0))

/*
** abdlb_u32_untied:
**	uabdlb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svabdlb_u32 (z1, z2),
		    z0_res = svabdlb (z1, z2))

/*
** abdlb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uabdlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svabdlb_n_u32 (z0, x0),
		     z0_res = svabdlb (z0, x0))

/*
** abdlb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uabdlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlb_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svabdlb_n_u32 (z1, x0),
		     z0_res = svabdlb (z1, x0))

/*
** abdlb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uabdlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svabdlb_n_u32 (z0, 11),
		    z0_res = svabdlb (z0, 11))

/*
** abdlb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uabdlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlb_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svabdlb_n_u32 (z1, 11),
		    z0_res = svabdlb (z1, 11))
