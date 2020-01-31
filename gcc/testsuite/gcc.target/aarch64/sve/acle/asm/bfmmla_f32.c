/* { dg-additional-options "-march=armv8.2-a+sve+bf16" } */
/* { dg-require-effective-target aarch64_asm_bf16_ok }  */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bfmmla_f32_tied1:
**	bfmmla	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmmla_f32_tied1, svfloat32_t, svbfloat16_t,
	     z0 = svbfmmla_f32 (z0, z4, z5),
	     z0 = svbfmmla (z0, z4, z5))

/*
** bfmmla_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmmla	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmmla_f32_tied2, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmmla_f32 (z4, z0, z1),
		 z0_res = svbfmmla (z4, z0, z1))

/*
** bfmmla_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmmla	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmmla_f32_tied3, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmmla_f32 (z4, z1, z0),
		 z0_res = svbfmmla (z4, z1, z0))

/*
** bfmmla_f32_untied:
**	movprfx	z0, z1
**	bfmmla	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmmla_f32_untied, svfloat32_t, svbfloat16_t,
	     z0 = svbfmmla_f32 (z1, z4, z5),
	     z0 = svbfmmla (z1, z4, z5))
