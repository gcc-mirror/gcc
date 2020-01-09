/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvtlt_f32_f16_m_tied1:
**	fcvtlt	z0\.s, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvtlt_f32_f16_m_tied1, svfloat32_t, svfloat16_t,
	     z0 = svcvtlt_f32_f16_m (z0, p0, z4),
	     z0 = svcvtlt_f32_m (z0, p0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (cvtlt_f32_f16_m_tied2, svfloat32_t, svfloat16_t,
		 z0_res = svcvtlt_f32_f16_m (z4, p0, z0),
		 z0_res = svcvtlt_f32_m (z4, p0, z0))

/*
** cvtlt_f32_f16_m_untied:
** (
**	mov	z0\.d, z1\.d
**	fcvtlt	z0\.s, p0/m, z4\.h
** |
**	fcvtlt	z1\.s, p0/m, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (cvtlt_f32_f16_m_untied, svfloat32_t, svfloat16_t,
	     z0 = svcvtlt_f32_f16_m (z1, p0, z4),
	     z0 = svcvtlt_f32_m (z1, p0, z4))

/*
** cvtlt_f32_f16_x_tied1:
**	fcvtlt	z0\.s, p0/m, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (cvtlt_f32_f16_x_tied1, svfloat32_t, svfloat16_t,
		 z0_res = svcvtlt_f32_f16_x (p0, z0),
		 z0_res = svcvtlt_f32_x (p0, z0))

/*
** cvtlt_f32_f16_x_untied:
**	fcvtlt	z0\.s, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvtlt_f32_f16_x_untied, svfloat32_t, svfloat16_t,
	     z0 = svcvtlt_f32_f16_x (p0, z4),
	     z0 = svcvtlt_f32_x (p0, z4))

/*
** ptrue_cvtlt_f32_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvtlt_f32_f16_x_tied1, svfloat32_t, svfloat16_t,
		 z0_res = svcvtlt_f32_f16_x (svptrue_b32 (), z0),
		 z0_res = svcvtlt_f32_x (svptrue_b32 (), z0))

/*
** ptrue_cvtlt_f32_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtlt_f32_f16_x_untied, svfloat32_t, svfloat16_t,
	     z0 = svcvtlt_f32_f16_x (svptrue_b32 (), z4),
	     z0 = svcvtlt_f32_x (svptrue_b32 (), z4))
