/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adalp_s64_m_tied1:
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_m_tied1, svint64_t, svint32_t,
	     z0 = svadalp_s64_m (p0, z0, z4),
	     z0 = svadalp_m (p0, z0, z4))

/*
** adalp_s64_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_s64_m_tied2, svint64_t, svint32_t,
		 z0_res = svadalp_s64_m (p0, z4, z0),
		 z0_res = svadalp_m (p0, z4, z0))

/*
** adalp_s64_m_untied:
**	movprfx	z0, z1
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_m_untied, svint64_t, svint32_t,
	     z0 = svadalp_s64_m (p0, z1, z4),
	     z0 = svadalp_m (p0, z1, z4))

/*
** adalp_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_z_tied1, svint64_t, svint32_t,
	     z0 = svadalp_s64_z (p0, z0, z4),
	     z0 = svadalp_z (p0, z0, z4))

/*
** adalp_s64_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.d, p0/z, z4\.d
**	sadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_s64_z_tied2, svint64_t, svint32_t,
		 z0_res = svadalp_s64_z (p0, z4, z0),
		 z0_res = svadalp_z (p0, z4, z0))

/*
** adalp_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_z_untied, svint64_t, svint32_t,
	     z0 = svadalp_s64_z (p0, z1, z4),
	     z0 = svadalp_z (p0, z1, z4))

/*
** adalp_s64_x_tied1:
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_x_tied1, svint64_t, svint32_t,
	     z0 = svadalp_s64_x (p0, z0, z4),
	     z0 = svadalp_x (p0, z0, z4))

/*
** adalp_s64_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_s64_x_tied2, svint64_t, svint32_t,
		 z0_res = svadalp_s64_x (p0, z4, z0),
		 z0_res = svadalp_x (p0, z4, z0))

/*
** adalp_s64_x_untied:
**	movprfx	z0, z1
**	sadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_s64_x_untied, svint64_t, svint32_t,
	     z0 = svadalp_s64_x (p0, z1, z4),
	     z0 = svadalp_x (p0, z1, z4))
