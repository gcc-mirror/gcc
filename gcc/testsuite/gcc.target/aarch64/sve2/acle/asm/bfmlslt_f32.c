/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** bfmlslt_f32_tied1:
**	bfmlslt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlslt_f32_tied1, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlslt_f32 (z0, z4, z5),
	     z0 = svbfmlslt (z0, z4, z5))

/*
** bfmlslt_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlslt	z0\.s, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlslt_f32_tied2, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlslt_f32 (z4, z0, z1),
		 z0_res = svbfmlslt (z4, z0, z1))

/*
** bfmlslt_f32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	bfmlslt	z0\.s, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (bfmlslt_f32_tied3, svfloat32_t, svbfloat16_t,
		 z0_res = svbfmlslt_f32 (z4, z1, z0),
		 z0_res = svbfmlslt (z4, z1, z0))

/*
** bfmlslt_f32_untied:
**	movprfx	z0, z1
**	bfmlslt	z0\.s, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (bfmlslt_f32_untied, svfloat32_t, svbfloat16_t,
	     z0 = svbfmlslt_f32 (z1, z4, z5),
	     z0 = svbfmlslt (z1, z4, z5))

/*
** bfmlslt_h7_f32_tied1:
**	mov	(z[0-9]+\.h), h7
**	bfmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlslt_h7_f32_tied1, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlslt_n_f32 (z0, z4, d7),
	      z0 = svbfmlslt (z0, z4, d7))

/*
** bfmlslt_h7_f32_untied:
**	mov	(z[0-9]+\.h), h7
**	movprfx	z0, z1
**	bfmlslt	z0\.s, z4\.h, \1
**	ret
*/
TEST_DUAL_ZD (bfmlslt_h7_f32_untied, svfloat32_t, svbfloat16_t, bfloat16_t,
	      z0 = svbfmlslt_n_f32 (z1, z4, d7),
	      z0 = svbfmlslt (z1, z4, d7))
