/* { dg-require-effective-target aarch64_asm_f32mm_ok } */
/* { dg-additional-options "-march=armv8.2-a+f32mm" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mmla_f32_tied1:
**	fmmla	z0\.s, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (mmla_f32_tied1, svfloat32_t, svfloat32_t,
	     z0 = svmmla_f32 (z0, z4, z5),
	     z0 = svmmla (z0, z4, z5))

/*
** mmla_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmmla	z0\.s, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (mmla_f32_tied2, svfloat32_t, svfloat32_t,
		 z0_res = svmmla_f32 (z4, z0, z1),
		 z0_res = svmmla (z4, z0, z1))

/*
** mmla_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmmla	z0\.s, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (mmla_f32_tied3, svfloat32_t, svfloat32_t,
		 z0_res = svmmla_f32 (z4, z1, z0),
		 z0_res = svmmla (z4, z1, z0))

/*
** mmla_f32_untied:
**	movprfx	z0, z1
**	fmmla	z0\.s, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (mmla_f32_untied, svfloat32_t, svfloat32_t,
	     z0 = svmmla_f32 (z1, z4, z5),
	     z0 = svmmla (z1, z4, z5))
