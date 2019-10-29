/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clz_s32_m_tied1:
**	clz	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (clz_s32_m_tied1, svuint32_t, svint32_t,
	     z0 = svclz_s32_m (z0, p0, z4),
	     z0 = svclz_m (z0, p0, z4))

/*
** clz_s32_m_untied:
**	movprfx	z0, z1
**	clz	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (clz_s32_m_untied, svuint32_t, svint32_t,
	     z0 = svclz_s32_m (z1, p0, z4),
	     z0 = svclz_m (z1, p0, z4))

/*
** clz_s32_z:
**	movprfx	z0\.s, p0/z, z4\.s
**	clz	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (clz_s32_z, svuint32_t, svint32_t,
	     z0 = svclz_s32_z (p0, z4),
	     z0 = svclz_z (p0, z4))

/*
** clz_s32_x:
**	clz	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (clz_s32_x, svuint32_t, svint32_t,
	     z0 = svclz_s32_x (p0, z4),
	     z0 = svclz_x (p0, z4))
