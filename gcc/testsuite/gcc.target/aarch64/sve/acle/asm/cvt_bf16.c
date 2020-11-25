/* { dg-additional-options "-march=armv8.2-a+sve+bf16" } */
/* { dg-require-effective-target aarch64_asm_bf16_ok }  */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvt_bf16_f32_m_tied1:
**	bfcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_bf16_f32_m_tied1, svbfloat16_t, svfloat32_t,
	     z0 = svcvt_bf16_f32_m (z0, p0, z4),
	     z0 = svcvt_bf16_m (z0, p0, z4))

/*
** cvt_bf16_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfcvt	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_bf16_f32_m_tied2, svbfloat16_t, svfloat32_t,
		 z0_res = svcvt_bf16_f32_m (z4, p0, z0),
		 z0_res = svcvt_bf16_m (z4, p0, z0))

/*
** cvt_bf16_f32_m_untied:
**	movprfx	z0, z1
**	bfcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_bf16_f32_m_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvt_bf16_f32_m (z1, p0, z4),
	     z0 = svcvt_bf16_m (z1, p0, z4))

/*
** cvt_bf16_f32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	bfcvt	z0\.h, p0/m, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_bf16_f32_z_tied1, svbfloat16_t, svfloat32_t,
		 z0_res = svcvt_bf16_f32_z (p0, z0),
		 z0_res = svcvt_bf16_z (p0, z0))

/*
** cvt_bf16_f32_z_untied:
**	movprfx	z0\.s, p0/z, z4\.s
**	bfcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_bf16_f32_z_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvt_bf16_f32_z (p0, z4),
	     z0 = svcvt_bf16_z (p0, z4))

/*
** cvt_bf16_f32_x_tied1:
**	bfcvt	z0\.h, p0/m, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (cvt_bf16_f32_x_tied1, svbfloat16_t, svfloat32_t,
		 z0_res = svcvt_bf16_f32_x (p0, z0),
		 z0_res = svcvt_bf16_x (p0, z0))

/*
** cvt_bf16_f32_x_untied:
**	movprfx	z0, z4
**	bfcvt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvt_bf16_f32_x_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvt_bf16_f32_x (p0, z4),
	     z0 = svcvt_bf16_x (p0, z4))

/*
** ptrue_cvt_bf16_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_cvt_bf16_f32_x_tied1, svbfloat16_t, svfloat32_t,
		 z0_res = svcvt_bf16_f32_x (svptrue_b32 (), z0),
		 z0_res = svcvt_bf16_x (svptrue_b32 (), z0))

/*
** ptrue_cvt_bf16_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvt_bf16_f32_x_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvt_bf16_f32_x (svptrue_b32 (), z4),
	     z0 = svcvt_bf16_x (svptrue_b32 (), z4))
