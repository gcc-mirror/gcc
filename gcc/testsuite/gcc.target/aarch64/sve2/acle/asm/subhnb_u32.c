/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnb_u32_tied1:
**	subhnb	z0\.h, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svsubhnb_u32 (z0, z1),
		    z0_res = svsubhnb (z0, z1))

/*
** subhnb_u32_tied2:
**	subhnb	z0\.h, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u32_tied2, svuint16_t, svuint32_t,
		    z0_res = svsubhnb_u32 (z1, z0),
		    z0_res = svsubhnb (z1, z0))

/*
** subhnb_u32_untied:
**	subhnb	z0\.h, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svsubhnb_u32 (z1, z2),
		    z0_res = svsubhnb (z1, z2))

/*
** subhnb_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	subhnb	z0\.h, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_u32_tied1, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svsubhnb_n_u32 (z0, x0),
		     z0_res = svsubhnb (z0, x0))

/*
** subhnb_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	subhnb	z0\.h, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_u32_untied, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svsubhnb_n_u32 (z1, x0),
		     z0_res = svsubhnb (z1, x0))

/*
** subhnb_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	subhnb	z0\.h, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svsubhnb_n_u32 (z0, 11),
		    z0_res = svsubhnb (z0, 11))

/*
** subhnb_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	subhnb	z0\.h, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svsubhnb_n_u32 (z1, 11),
		    z0_res = svsubhnb (z1, 11))
