/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlalb_f32_tied1:
**	fmlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalb_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlalb_f32 (z0, z4, z5),
	     z0 = svmlalb (z0, z4, z5))

/*
** mlalb_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalb_f32_tied2, svfloat32_t, svfloat16_t,
		 z0_res = svmlalb_f32 (z4, z0, z1),
		 z0_res = svmlalb (z4, z0, z1))

/*
** mlalb_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlalb	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlalb_f32_tied3, svfloat32_t, svfloat16_t,
		 z0_res = svmlalb_f32 (z4, z1, z0),
		 z0_res = svmlalb (z4, z1, z0))

/*
** mlalb_f32_untied:
**	movprfx	z0, z1
**	fmlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlalb_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlalb_f32 (z1, z4, z5),
	     z0 = svmlalb (z1, z4, z5))

/*
** mlalb_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	fmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f32_tied1, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlalb_n_f32 (z0, z4, d7),
	      z0 = svmlalb (z0, z4, d7))

/*
** mlalb_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	fmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlalb_h7_f32_untied, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlalb_n_f32 (z1, z4, d7),
	      z0 = svmlalb (z1, z4, d7))

/*
** mlalb_2_f32_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalb_2_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlalb_n_f32 (z0, z4, 2),
	     z0 = svmlalb (z0, z4, 2))

/*
** mlalb_2_f32_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlalb_2_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlalb_n_f32 (z1, z4, 2),
	     z0 = svmlalb (z1, z4, 2))
