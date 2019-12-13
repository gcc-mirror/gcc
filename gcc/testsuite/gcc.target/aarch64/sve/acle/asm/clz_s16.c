/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clz_s16_m_tied1:
**	clz	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (clz_s16_m_tied1, svuint16_t, svint16_t,
	     z0 = svclz_s16_m (z0, p0, z4),
	     z0 = svclz_m (z0, p0, z4))

/*
** clz_s16_m_untied:
**	movprfx	z0, z1
**	clz	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (clz_s16_m_untied, svuint16_t, svint16_t,
	     z0 = svclz_s16_m (z1, p0, z4),
	     z0 = svclz_m (z1, p0, z4))

/*
** clz_s16_z:
**	movprfx	z0\.h, p0/z, z4\.h
**	clz	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (clz_s16_z, svuint16_t, svint16_t,
	     z0 = svclz_s16_z (p0, z4),
	     z0 = svclz_z (p0, z4))

/*
** clz_s16_x:
**	clz	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (clz_s16_x, svuint16_t, svint16_t,
	     z0 = svclz_s16_x (p0, z4),
	     z0 = svclz_x (p0, z4))
