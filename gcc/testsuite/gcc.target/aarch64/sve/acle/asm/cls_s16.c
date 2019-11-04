/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cls_s16_m_tied1:
**	cls	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cls_s16_m_tied1, svuint16_t, svint16_t,
	     z0 = svcls_s16_m (z0, p0, z4),
	     z0 = svcls_m (z0, p0, z4))

/*
** cls_s16_m_untied:
**	movprfx	z0, z1
**	cls	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cls_s16_m_untied, svuint16_t, svint16_t,
	     z0 = svcls_s16_m (z1, p0, z4),
	     z0 = svcls_m (z1, p0, z4))

/*
** cls_s16_z:
**	movprfx	z0\.h, p0/z, z4\.h
**	cls	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cls_s16_z, svuint16_t, svint16_t,
	     z0 = svcls_s16_z (p0, z4),
	     z0 = svcls_z (p0, z4))

/*
** cls_s16_x:
**	cls	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cls_s16_x, svuint16_t, svint16_t,
	     z0 = svcls_s16_x (p0, z4),
	     z0 = svcls_x (p0, z4))
