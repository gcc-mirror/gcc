/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvtx_f32_f64_m_tied1:
**	fcvtx	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtx_f32_f64_m_tied1, svfloat32_t, svfloat64_t,
	     z0 = svcvtx_f32_f64_m (z0, p0, z4),
	     z0 = svcvtx_f32_m (z0, p0, z4))

/*
** cvtx_f32_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	fcvtx	z0\.s, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvtx_f32_f64_m_tied2, svfloat32_t, svfloat64_t,
		 z0_res = svcvtx_f32_f64_m (z4, p0, z0),
		 z0_res = svcvtx_f32_m (z4, p0, z0))

/*
** cvtx_f32_f64_m_untied:
**	movprfx	z0, z1
**	fcvtx	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtx_f32_f64_m_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtx_f32_f64_m (z1, p0, z4),
	     z0 = svcvtx_f32_m (z1, p0, z4))

/*
** cvtx_f32_f64_z_tied1:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	fcvtx	z0\.s, p0/m, \1
**	ret
*/
TEST_DUAL_Z_REV (cvtx_f32_f64_z_tied1, svfloat32_t, svfloat64_t,
		 z0_res = svcvtx_f32_f64_z (p0, z0),
		 z0_res = svcvtx_f32_z (p0, z0))

/*
** cvtx_f32_f64_z_untied:
**	movprfx	z0\.d, p0/z, z4\.d
**	fcvtx	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtx_f32_f64_z_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtx_f32_f64_z (p0, z4),
	     z0 = svcvtx_f32_z (p0, z4))

/*
** cvtx_f32_f64_x_tied1:
**	fcvtx	z0\.s, p0/m, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (cvtx_f32_f64_x_tied1, svfloat32_t, svfloat64_t,
		 z0_res = svcvtx_f32_f64_x (p0, z0),
		 z0_res = svcvtx_f32_x (p0, z0))

/*
** cvtx_f32_f64_x_untied:
**	movprfx	z0, z4
**	fcvtx	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtx_f32_f64_x_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtx_f32_f64_x (p0, z4),
	     z0 = svcvtx_f32_x (p0, z4))

/*
** ptrue_cvtx_f32_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvtx_f32_f64_x_tied1, svfloat32_t, svfloat64_t,
		 z0_res = svcvtx_f32_f64_x (svptrue_b64 (), z0),
		 z0_res = svcvtx_f32_x (svptrue_b64 (), z0))

/*
** ptrue_cvtx_f32_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtx_f32_f64_x_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtx_f32_f64_x (svptrue_b64 (), z4),
	     z0 = svcvtx_f32_x (svptrue_b64 (), z4))
