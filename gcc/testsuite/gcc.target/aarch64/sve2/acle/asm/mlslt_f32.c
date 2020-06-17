/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mlslt_f32_tied1:
**	fmlslt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlslt_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlslt_f32 (z0, z4, z5),
	     z0 = svmlslt (z0, z4, z5))

/*
** mlslt_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlslt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlslt_f32_tied2, svfloat32_t, svfloat16_t,
		 z0_res = svmlslt_f32 (z4, z0, z1),
		 z0_res = svmlslt (z4, z0, z1))

/*
** mlslt_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fmlslt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (mlslt_f32_tied3, svfloat32_t, svfloat16_t,
		 z0_res = svmlslt_f32 (z4, z1, z0),
		 z0_res = svmlslt (z4, z1, z0))

/*
** mlslt_f32_untied:
**	movprfx	z0, z1
**	fmlslt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (mlslt_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlslt_f32 (z1, z4, z5),
	     z0 = svmlslt (z1, z4, z5))

/*
** mlslt_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	fmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlslt_h7_f32_tied1, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlslt_n_f32 (z0, z4, d7),
	      z0 = svmlslt (z0, z4, d7))

/*
** mlslt_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	fmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (mlslt_h7_f32_untied, svfloat32_t, svfloat16_t, float16_t,
	      z0 = svmlslt_n_f32 (z1, z4, d7),
	      z0 = svmlslt (z1, z4, d7))

/*
** mlslt_2_f32_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlslt_2_f32_tied1, svfloat32_t, svfloat16_t,
	     z0 = svmlslt_n_f32 (z0, z4, 2),
	     z0 = svmlslt (z0, z4, 2))

/*
** mlslt_2_f32_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (mlslt_2_f32_untied, svfloat32_t, svfloat16_t,
	     z0 = svmlslt_n_f32 (z1, z4, 2),
	     z0 = svmlslt (z1, z4, 2))
