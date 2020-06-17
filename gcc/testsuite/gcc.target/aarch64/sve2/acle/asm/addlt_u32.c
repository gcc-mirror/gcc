/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlt_u32_tied1:
**	uaddlt	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svaddlt_u32 (z0, z1),
		    z0_res = svaddlt (z0, z1))

/*
** addlt_u32_tied2:
**	uaddlt	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svaddlt_u32 (z1, z0),
		    z0_res = svaddlt (z1, z0))

/*
** addlt_u32_untied:
**	uaddlt	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svaddlt_u32 (z1, z2),
		    z0_res = svaddlt (z1, z2))

/*
** addlt_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uaddlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svaddlt_n_u32 (z0, x0),
		     z0_res = svaddlt (z0, x0))

/*
** addlt_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uaddlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlt_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svaddlt_n_u32 (z1, x0),
		     z0_res = svaddlt (z1, x0))

/*
** addlt_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uaddlt	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svaddlt_n_u32 (z0, 11),
		    z0_res = svaddlt (z0, 11))

/*
** addlt_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uaddlt	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlt_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svaddlt_n_u32 (z1, 11),
		    z0_res = svaddlt (z1, 11))
