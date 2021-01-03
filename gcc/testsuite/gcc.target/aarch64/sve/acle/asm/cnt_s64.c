/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnt_s64_m_tied1:
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_s64_m_tied1, svuint64_t, svint64_t,
	     z0 = svcnt_s64_m (z0, p0, z4),
	     z0 = svcnt_m (z0, p0, z4))

/*
** cnt_s64_m_untied:
**	movprfx	z0, z1
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_s64_m_untied, svuint64_t, svint64_t,
	     z0 = svcnt_s64_m (z1, p0, z4),
	     z0 = svcnt_m (z1, p0, z4))

/*
** cnt_s64_z:
**	movprfx	z0\.d, p0/z, z4\.d
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_s64_z, svuint64_t, svint64_t,
	     z0 = svcnt_s64_z (p0, z4),
	     z0 = svcnt_z (p0, z4))

/*
** cnt_s64_x:
**	movprfx	z0, z4
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_s64_x, svuint64_t, svint64_t,
	     z0 = svcnt_s64_x (p0, z4),
	     z0 = svcnt_x (p0, z4))
