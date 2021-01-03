/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clz_s8_m_tied1:
**	clz	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (clz_s8_m_tied1, svuint8_t, svint8_t,
	     z0 = svclz_s8_m (z0, p0, z4),
	     z0 = svclz_m (z0, p0, z4))

/*
** clz_s8_m_untied:
**	movprfx	z0, z1
**	clz	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (clz_s8_m_untied, svuint8_t, svint8_t,
	     z0 = svclz_s8_m (z1, p0, z4),
	     z0 = svclz_m (z1, p0, z4))

/*
** clz_s8_z:
**	movprfx	z0\.b, p0/z, z4\.b
**	clz	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (clz_s8_z, svuint8_t, svint8_t,
	     z0 = svclz_s8_z (p0, z4),
	     z0 = svclz_z (p0, z4))

/*
** clz_s8_x:
**	movprfx	z0, z4
**	clz	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (clz_s8_x, svuint8_t, svint8_t,
	     z0 = svclz_s8_x (p0, z4),
	     z0 = svclz_x (p0, z4))
