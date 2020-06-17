/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlslb_f32_tied1:
**	fmlslb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlslb_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlslb_f32 (z0, z4, z5),
	     z0 = svmlslb (z0, z4, z5))

/*
** mlslb_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlslb	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlslb_f32_tied2, svfloat32_t, svfloat16_t,
		 z0_res = svmlslb_f32 (z4, z0, z1),
		 z0_res = svmlslb (z4, z0, z1))

/*
** mlslb_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlslb	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlslb_f32_tied3, svfloat32_t, svfloat16_t,
		 z0_res = svmlslb_f32 (z4, z1, z0),
		 z0_res = svmlslb (z4, z1, z0))

/*
** mlslb_f32_untied:
**	movprfx	z0, z1
**	fmlslb	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlslb_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlslb_f32 (z1, z4, z5),
	     z0 = svmlslb (z1, z4, z5))

/*
** mlslb_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	fmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlslb_h7_f32_tied1, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlslb_n_f32 (z0, z4, d7),
	      z0 = svmlslb (z0, z4, d7))

/*
** mlslb_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	fmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlslb_h7_f32_untied, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlslb_n_f32 (z1, z4, d7),
	      z0 = svmlslb (z1, z4, d7))

/*
** mlslb_2_f32_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlslb_2_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlslb_n_f32 (z0, z4, 2),
	     z0 = svmlslb (z0, z4, 2))

/*
** mlslb_2_f32_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmlslb	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlslb_2_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlslb_n_f32 (z1, z4, 2),
	     z0 = svmlslb (z1, z4, 2))
