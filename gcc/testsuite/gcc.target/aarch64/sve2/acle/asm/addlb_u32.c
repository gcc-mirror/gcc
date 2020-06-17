/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlb_u32_tied1:
**	uaddlb	z0\.s, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svaddlb_u32 (z0, z1),
		    z0_res = svaddlb (z0, z1))

/*
** addlb_u32_tied2:
**	uaddlb	z0\.s, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_u32_tied2, svuint32_t, svuint16_t,
		    z0_res = svaddlb_u32 (z1, z0),
		    z0_res = svaddlb (z1, z0))

/*
** addlb_u32_untied:
**	uaddlb	z0\.s, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svaddlb_u32 (z1, z2),
		    z0_res = svaddlb (z1, z2))

/*
** addlb_w0_u32_tied1:
**	mov	(z[0-9]+\.h), w0
**	uaddlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlb_w0_u32_tied1, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svaddlb_n_u32 (z0, x0),
		     z0_res = svaddlb (z0, x0))

/*
** addlb_w0_u32_untied:
**	mov	(z[0-9]+\.h), w0
**	uaddlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlb_w0_u32_untied, svuint32_t, svuint16_t, uint16_t,
		     z0_res = svaddlb_n_u32 (z1, x0),
		     z0_res = svaddlb (z1, x0))

/*
** addlb_11_u32_tied1:
**	mov	(z[0-9]+\.h), #11
**	uaddlb	z0\.s, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_11_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svaddlb_n_u32 (z0, 11),
		    z0_res = svaddlb (z0, 11))

/*
** addlb_11_u32_untied:
**	mov	(z[0-9]+\.h), #11
**	uaddlb	z0\.s, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_11_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svaddlb_n_u32 (z1, 11),
		    z0_res = svaddlb (z1, 11))
