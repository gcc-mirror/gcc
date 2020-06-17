/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvtnt_f32_f64_m_tied1:
**	fcvtnt	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtnt_f32_f64_m_tied1, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_m (z0, p0, z4),
	     z0 = svcvtnt_f32_m (z0, p0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (cvtnt_f32_f64_m_tied2, svfloat32_t, svfloat64_t,
		 z0_res = svcvtnt_f32_f64_m (z4, p0, z0),
		 z0_res = svcvtnt_f32_m (z4, p0, z0))

/*
** cvtnt_f32_f64_m_untied:
** (
**	mov	z0\.d, z1\.d
**	fcvtnt	z0\.s, p0/m, z4\.d
** |
**	fcvtnt	z1\.s, p0/m, z4\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (cvtnt_f32_f64_m_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_m (z1, p0, z4),
	     z0 = svcvtnt_f32_m (z1, p0, z4))

/*
** cvtnt_f32_f64_x_tied1:
**	fcvtnt	z0\.s, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cvtnt_f32_f64_x_tied1, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_x (z0, p0, z4),
	     z0 = svcvtnt_f32_x (z0, p0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (cvtnt_f32_f64_x_tied2, svfloat32_t, svfloat64_t,
		 z0_res = svcvtnt_f32_f64_x (z4, p0, z0),
		 z0_res = svcvtnt_f32_x (z4, p0, z0))

/*
** cvtnt_f32_f64_x_untied:
** (
**	mov	z0\.d, z1\.d
**	fcvtnt	z0\.s, p0/m, z4\.d
** |
**	fcvtnt	z1\.s, p0/m, z4\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (cvtnt_f32_f64_x_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_x (z1, p0, z4),
	     z0 = svcvtnt_f32_x (z1, p0, z4))

/*
** ptrue_cvtnt_f32_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtnt_f32_f64_x_tied1, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_x (z0, svptrue_b64 (), z4),
	     z0 = svcvtnt_f32_x (z0, svptrue_b64 (), z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (ptrue_cvtnt_f32_f64_x_tied2, svfloat32_t, svfloat64_t,
		 z0_res = svcvtnt_f32_f64_x (z4, svptrue_b64 (), z0),
		 z0_res = svcvtnt_f32_x (z4, svptrue_b64 (), z0))

/*
** ptrue_cvtnt_f32_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtnt_f32_f64_x_untied, svfloat32_t, svfloat64_t,
	     z0 = svcvtnt_f32_f64_x (z1, svptrue_b64 (), z4),
	     z0 = svcvtnt_f32_x (z1, svptrue_b64 (), z4))
