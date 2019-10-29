/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnt_s8_m_tied1:
**	cnt	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cnt_s8_m_tied1, svuint8_t, svint8_t,
	     z0 = svcnt_s8_m (z0, p0, z4),
	     z0 = svcnt_m (z0, p0, z4))

/*
** cnt_s8_m_untied:
**	movprfx	z0, z1
**	cnt	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cnt_s8_m_untied, svuint8_t, svint8_t,
	     z0 = svcnt_s8_m (z1, p0, z4),
	     z0 = svcnt_m (z1, p0, z4))

/*
** cnt_s8_z:
**	movprfx	z0\.b, p0/z, z4\.b
**	cnt	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cnt_s8_z, svuint8_t, svint8_t,
	     z0 = svcnt_s8_z (p0, z4),
	     z0 = svcnt_z (p0, z4))

/*
** cnt_s8_x:
**	cnt	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cnt_s8_x, svuint8_t, svint8_t,
	     z0 = svcnt_s8_x (p0, z4),
	     z0 = svcnt_x (p0, z4))
