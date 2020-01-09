/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnb_u32_tied1:
**	rsubhnb	z0\.h, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svrsubhnb_u32 (z0, z1),
		    z0_res = svrsubhnb (z0, z1))

/*
** rsubhnb_u32_tied2:
**	rsubhnb	z0\.h, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_u32_tied2, svuint16_t, svuint32_t,
		    z0_res = svrsubhnb_u32 (z1, z0),
		    z0_res = svrsubhnb (z1, z0))

/*
** rsubhnb_u32_untied:
**	rsubhnb	z0\.h, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svrsubhnb_u32 (z1, z2),
		    z0_res = svrsubhnb (z1, z2))

/*
** rsubhnb_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	rsubhnb	z0\.h, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_w0_u32_tied1, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svrsubhnb_n_u32 (z0, x0),
		     z0_res = svrsubhnb (z0, x0))

/*
** rsubhnb_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	rsubhnb	z0\.h, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_w0_u32_untied, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svrsubhnb_n_u32 (z1, x0),
		     z0_res = svrsubhnb (z1, x0))

/*
** rsubhnb_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	rsubhnb	z0\.h, z0\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svrsubhnb_n_u32 (z0, 11),
		    z0_res = svrsubhnb (z0, 11))

/*
** rsubhnb_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	rsubhnb	z0\.h, z1\.s, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svrsubhnb_n_u32 (z1, 11),
		    z0_res = svrsubhnb (z1, 11))
