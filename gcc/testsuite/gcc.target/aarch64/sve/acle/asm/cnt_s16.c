/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnt_s16_m_tied1:
**	cnt	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cnt_s16_m_tied1, svuint16_t, svint16_t,
	     z0 = svcnt_s16_m (z0, p0, z4),
	     z0 = svcnt_m (z0, p0, z4))

/*
** cnt_s16_m_untied:
**	movprfx	z0, z1
**	cnt	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cnt_s16_m_untied, svuint16_t, svint16_t,
	     z0 = svcnt_s16_m (z1, p0, z4),
	     z0 = svcnt_m (z1, p0, z4))

/*
** cnt_s16_z:
**	movprfx	z0\.h, p0/z, z4\.h
**	cnt	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cnt_s16_z, svuint16_t, svint16_t,
	     z0 = svcnt_s16_z (p0, z4),
	     z0 = svcnt_z (p0, z4))

/*
** cnt_s16_x:
**	movprfx	z0, z4
**	cnt	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cnt_s16_x, svuint16_t, svint16_t,
	     z0 = svcnt_s16_x (p0, z4),
	     z0 = svcnt_x (p0, z4))
