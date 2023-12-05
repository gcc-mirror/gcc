/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** bfmlalb_f32_tied1:
**	bfmlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlalb_f32_tied1, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlalb_f32 (z0, z4, z5),
	     z0 = svbfmlalb (z0, z4, z5))

/*
** bfmlalb_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlalb	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlalb_f32_tied2, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlalb_f32 (z4, z0, z1),
		 z0_res = svbfmlalb (z4, z0, z1))

/*
** bfmlalb_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlalb	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlalb_f32_tied3, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlalb_f32 (z4, z1, z0),
		 z0_res = svbfmlalb (z4, z1, z0))

/*
** bfmlalb_f32_untied:
**	movprfx	z0, z1
**	bfmlalb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlalb_f32_untied, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlalb_f32 (z1, z4, z5),
	     z0 = svbfmlalb (z1, z4, z5))

/*
** bfmlalb_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	bfmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlalb_h7_f32_tied1, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlalb_n_f32 (z0, z4, d7),
	      z0 = svbfmlalb (z0, z4, d7))

/*
** bfmlalb_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	bfmlalb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlalb_h7_f32_untied, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlalb_n_f32 (z1, z4, d7),
	      z0 = svbfmlalb (z1, z4, d7))
