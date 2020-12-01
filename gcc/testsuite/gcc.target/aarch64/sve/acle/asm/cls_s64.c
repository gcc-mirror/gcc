/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cls_s64_m_tied1:
**	cls	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cls_s64_m_tied1, svuint64_t, svint64_t,
	     z0 = svcls_s64_m (z0, p0, z4),
	     z0 = svcls_m (z0, p0, z4))

/*
** cls_s64_m_untied:
**	movprfx	z0, z1
**	cls	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cls_s64_m_untied, svuint64_t, svint64_t,
	     z0 = svcls_s64_m (z1, p0, z4),
	     z0 = svcls_m (z1, p0, z4))

/*
** cls_s64_z:
**	movprfx	z0\.d, p0/z, z4\.d
**	cls	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cls_s64_z, svuint64_t, svint64_t,
	     z0 = svcls_s64_z (p0, z4),
	     z0 = svcls_z (p0, z4))

/*
** cls_s64_x:
**	movprfx	z0, z4
**	cls	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cls_s64_x, svuint64_t, svint64_t,
	     z0 = svcls_s64_x (p0, z4),
	     z0 = svcls_x (p0, z4))
