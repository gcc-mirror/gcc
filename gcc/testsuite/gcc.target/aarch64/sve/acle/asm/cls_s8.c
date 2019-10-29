/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cls_s8_m_tied1:
**	cls	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cls_s8_m_tied1, svuint8_t, svint8_t,
	     z0 = svcls_s8_m (z0, p0, z4),
	     z0 = svcls_m (z0, p0, z4))

/*
** cls_s8_m_untied:
**	movprfx	z0, z1
**	cls	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cls_s8_m_untied, svuint8_t, svint8_t,
	     z0 = svcls_s8_m (z1, p0, z4),
	     z0 = svcls_m (z1, p0, z4))

/*
** cls_s8_z:
**	movprfx	z0\.b, p0/z, z4\.b
**	cls	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cls_s8_z, svuint8_t, svint8_t,
	     z0 = svcls_s8_z (p0, z4),
	     z0 = svcls_z (p0, z4))

/*
** cls_s8_x:
**	cls	z0\.b, p0/m, z4\.b
**	ret
*/
TEST_DUAL_Z (cls_s8_x, svuint8_t, svint8_t,
	     z0 = svcls_s8_x (p0, z4),
	     z0 = svcls_x (p0, z4))
