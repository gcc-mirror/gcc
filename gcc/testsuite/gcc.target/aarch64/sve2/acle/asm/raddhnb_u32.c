/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** raddhnb_u32_tied1:
**	raddhnb	z0\.h, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svraddhnb_u32 (z0, z1),
		    z0_res = svraddhnb (z0, z1))

/*
** raddhnb_u32_tied2:
**	raddhnb	z0\.h, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_u32_tied2, svuint16_t, svuint32_t,
		    z0_res = svraddhnb_u32 (z1, z0),
		    z0_res = svraddhnb (z1, z0))

/*
** raddhnb_u32_untied:
**	raddhnb	z0\.h, (z1\.s, z2\.s|z2\.s, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svraddhnb_u32 (z1, z2),
		    z0_res = svraddhnb (z1, z2))

/*
** raddhnb_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	raddhnb	z0\.h, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_ZX (raddhnb_w0_u32_tied1, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svraddhnb_n_u32 (z0, x0),
		     z0_res = svraddhnb (z0, x0))

/*
** raddhnb_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	raddhnb	z0\.h, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_ZX (raddhnb_w0_u32_untied, svuint16_t, svuint32_t, uint32_t,
		     z0_res = svraddhnb_n_u32 (z1, x0),
		     z0_res = svraddhnb (z1, x0))

/*
** raddhnb_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	raddhnb	z0\.h, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_11_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svraddhnb_n_u32 (z0, 11),
		    z0_res = svraddhnb (z0, 11))

/*
** raddhnb_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
**	raddhnb	z0\.h, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_11_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svraddhnb_n_u32 (z1, 11),
		    z0_res = svraddhnb (z1, 11))
