/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlt_u32_tied1:
**	uabdlt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svabdlt_u32 (z0, z1),
		    z0_res = svabdlt (z0, z1))

/*
** abdlt_u32_tied2:
**	uabdlt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svabdlt_u32 (z1, z0),
		    z0_res = svabdlt (z1, z0))

/*
** abdlt_u32_untied:
**	uabdlt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svabdlt_u32 (z1, z2),
		    z0_res = svabdlt (z1, z2))

/*
** abdlt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uabdlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svabdlt_n_u32 (z0, x0),
		     z0_res = svabdlt (z0, x0))

/*
** abdlt_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uabdlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svabdlt_n_u32 (z1, x0),
		     z0_res = svabdlt (z1, x0))

/*
** abdlt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uabdlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svabdlt_n_u32 (z0, 11),
		    z0_res = svabdlt (z0, 11))

/*
** abdlt_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uabdlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svabdlt_n_u32 (z1, 11),
		    z0_res = svabdlt (z1, 11))
