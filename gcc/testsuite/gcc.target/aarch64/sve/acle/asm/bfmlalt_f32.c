/* { dg-additional-options "-march=armv8.2-a+sve+bf16" } */
/* { dg-require-effective-target aarch64_asm_bf16_ok }  */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bfmlalt_f32_tied1:
**	bfmlalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlalt_f32_tied1, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlalt_f32 (z0, z4, z5),
	     z0 = svbfmlalt (z0, z4, z5))

/*
** bfmlalt_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlalt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlalt_f32_tied2, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlalt_f32 (z4, z0, z1),
		 z0_res = svbfmlalt (z4, z0, z1))

/*
** bfmlalt_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlalt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlalt_f32_tied3, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlalt_f32 (z4, z1, z0),
		 z0_res = svbfmlalt (z4, z1, z0))

/*
** bfmlalt_f32_untied:
**	movprfx	z0, z1
**	bfmlalt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlalt_f32_untied, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlalt_f32 (z1, z4, z5),
	     z0 = svbfmlalt (z1, z4, z5))

/*
** bfmlalt_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	bfmlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlalt_h7_f32_tied1, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlalt_n_f32 (z0, z4, d7),
	      z0 = svbfmlalt (z0, z4, d7))

/*
** bfmlalt_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	bfmlalt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlalt_h7_f32_untied, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlalt_n_f32 (z1, z4, d7),
	      z0 = svbfmlalt (z1, z4, d7))
