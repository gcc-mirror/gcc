/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** bfmlslb_f32_tied1:
**	bfmlslb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlslb_f32_tied1, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlslb_f32 (z0, z4, z5),
	     z0 = svbfmlslb (z0, z4, z5))

/*
** bfmlslb_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlslb	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlslb_f32_tied2, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlslb_f32 (z4, z0, z1),
		 z0_res = svbfmlslb (z4, z0, z1))

/*
** bfmlslb_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlslb	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlslb_f32_tied3, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlslb_f32 (z4, z1, z0),
		 z0_res = svbfmlslb (z4, z1, z0))

/*
** bfmlslb_f32_untied:
**	movprfx	z0, z1
**	bfmlslb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlslb_f32_untied, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlslb_f32 (z1, z4, z5),
	     z0 = svbfmlslb (z1, z4, z5))

/*
** bfmlslb_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	bfmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlslb_h7_f32_tied1, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlslb_n_f32 (z0, z4, d7),
	      z0 = svbfmlslb (z0, z4, d7))

/*
** bfmlslb_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	bfmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlslb_h7_f32_untied, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlslb_n_f32 (z1, z4, d7),
	      z0 = svbfmlslb (z1, z4, d7))
