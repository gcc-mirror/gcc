/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adalp_u16_m_tied1:
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_m_tied1, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_m (p0, z0, z4),
	     z0 = svadalp_m (p0, z0, z4))

/*
** adalp_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uadalp	z0\.h, p0/m, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (adalp_u16_m_tied2, svuint16_t, svuint8_t,
		 z0_res = svadalp_u16_m (p0, z4, z0),
		 z0_res = svadalp_m (p0, z4, z0))

/*
** adalp_u16_m_untied:
**	movprfx	z0, z1
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_m_untied, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_m (p0, z1, z4),
	     z0 = svadalp_m (p0, z1, z4))

/*
** adalp_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_z_tied1, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_z (p0, z0, z4),
	     z0 = svadalp_z (p0, z0, z4))

/*
** adalp_u16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z4\.h
**	uadalp	z0\.h, p0/m, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (adalp_u16_z_tied2, svuint16_t, svuint8_t,
		 z0_res = svadalp_u16_z (p0, z4, z0),
		 z0_res = svadalp_z (p0, z4, z0))

/*
** adalp_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_z_untied, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_z (p0, z1, z4),
	     z0 = svadalp_z (p0, z1, z4))

/*
** adalp_u16_x_tied1:
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_x_tied1, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_x (p0, z0, z4),
	     z0 = svadalp_x (p0, z0, z4))

/*
** adalp_u16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uadalp	z0\.h, p0/m, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (adalp_u16_x_tied2, svuint16_t, svuint8_t,
		 z0_res = svadalp_u16_x (p0, z4, z0),
		 z0_res = svadalp_x (p0, z4, z0))

/*
** adalp_u16_x_untied:
**	movprfx	z0, z1
**	uadalp	z0\.h, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (adalp_u16_x_untied, svuint16_t, svuint8_t,
	     z0 = svadalp_u16_x (p0, z1, z4),
	     z0 = svadalp_x (p0, z1, z4))
