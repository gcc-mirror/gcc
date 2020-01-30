/* { dg-additional-options "-march=armv8.2-a+sve+bf16" } */
/* { dg-require-effective-target aarch64_asm_bf16_ok }  */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvtnt_bf16_f32_m_tied1:
**	bfcvtnt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvtnt_bf16_f32_m_tied1, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_m (z0, p0, z4),
	     z0 = svcvtnt_bf16_m (z0, p0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (cvtnt_bf16_f32_m_tied2, svbfloat16_t, svfloat32_t,
		 z0_res = svcvtnt_bf16_f32_m (z4, p0, z0),
		 z0_res = svcvtnt_bf16_m (z4, p0, z0))

/*
** cvtnt_bf16_f32_m_untied:
** (
**	mov	z0\.d, z1\.d
**	bfcvtnt	z0\.h, p0/m, z4\.s
** |
**	bfcvtnt	z1\.h, p0/m, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (cvtnt_bf16_f32_m_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_m (z1, p0, z4),
	     z0 = svcvtnt_bf16_m (z1, p0, z4))

/*
** cvtnt_bf16_f32_x_tied1:
**	bfcvtnt	z0\.h, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cvtnt_bf16_f32_x_tied1, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_x (z0, p0, z4),
	     z0 = svcvtnt_bf16_x (z0, p0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (cvtnt_bf16_f32_x_tied2, svbfloat16_t, svfloat32_t,
		 z0_res = svcvtnt_bf16_f32_x (z4, p0, z0),
		 z0_res = svcvtnt_bf16_x (z4, p0, z0))

/*
** cvtnt_bf16_f32_x_untied:
** (
**	mov	z0\.d, z1\.d
**	bfcvtnt	z0\.h, p0/m, z4\.s
** |
**	bfcvtnt	z1\.h, p0/m, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (cvtnt_bf16_f32_x_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_x (z1, p0, z4),
	     z0 = svcvtnt_bf16_x (z1, p0, z4))

/*
** ptrue_cvtnt_bf16_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtnt_bf16_f32_x_tied1, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_x (z0, svptrue_b32 (), z4),
	     z0 = svcvtnt_bf16_x (z0, svptrue_b32 (), z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (ptrue_cvtnt_bf16_f32_x_tied2, svbfloat16_t, svfloat32_t,
		 z0_res = svcvtnt_bf16_f32_x (z4, svptrue_b32 (), z0),
		 z0_res = svcvtnt_bf16_x (z4, svptrue_b32 (), z0))

/*
** ptrue_cvtnt_bf16_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cvtnt_bf16_f32_x_untied, svbfloat16_t, svfloat32_t,
	     z0 = svcvtnt_bf16_f32_x (z1, svptrue_b32 (), z4),
	     z0 = svcvtnt_bf16_x (z1, svptrue_b32 (), z4))
