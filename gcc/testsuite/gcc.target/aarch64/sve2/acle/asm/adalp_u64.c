/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adalp_u64_m_tied1:
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_m_tied1, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_m (p0, z0, z4),
	     z0 = svadalp_m (p0, z0, z4))

/*
** adalp_u64_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_u64_m_tied2, svuint64_t, svuint32_t,
		 z0_res = svadalp_u64_m (p0, z4, z0),
		 z0_res = svadalp_m (p0, z4, z0))

/*
** adalp_u64_m_untied:
**	movprfx	z0, z1
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_m_untied, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_m (p0, z1, z4),
	     z0 = svadalp_m (p0, z1, z4))

/*
** adalp_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_z_tied1, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_z (p0, z0, z4),
	     z0 = svadalp_z (p0, z0, z4))

/*
** adalp_u64_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.d, p0/z, z4\.d
**	uadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_u64_z_tied2, svuint64_t, svuint32_t,
		 z0_res = svadalp_u64_z (p0, z4, z0),
		 z0_res = svadalp_z (p0, z4, z0))

/*
** adalp_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_z_untied, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_z (p0, z1, z4),
	     z0 = svadalp_z (p0, z1, z4))

/*
** adalp_u64_x_tied1:
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_x_tied1, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_x (p0, z0, z4),
	     z0 = svadalp_x (p0, z0, z4))

/*
** adalp_u64_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	uadalp	z0\.d, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (adalp_u64_x_tied2, svuint64_t, svuint32_t,
		 z0_res = svadalp_u64_x (p0, z4, z0),
		 z0_res = svadalp_x (p0, z4, z0))

/*
** adalp_u64_x_untied:
**	movprfx	z0, z1
**	uadalp	z0\.d, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (adalp_u64_x_untied, svuint64_t, svuint32_t,
	     z0 = svadalp_u64_x (p0, z1, z4),
	     z0 = svadalp_x (p0, z1, z4))
