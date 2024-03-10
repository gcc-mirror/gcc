/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_f32_tied1:
**	clastb	z0\.s, p0, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_f32_tied1, svfloat32_t,
		z0 = svclastb_f32 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_f32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clastb	z0\.s, p0, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_f32_tied2, svfloat32_t,
		z0 = svclastb_f32 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_f32_untied:
**	movprfx	z0, z1
**	clastb	z0\.s, p0, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_f32_untied, svfloat32_t,
		z0 = svclastb_f32 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_d0_f32:
**	clastb	s0, p0, s0, z2\.s
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d0_f32, float32_t, svfloat32_t,
		  d0 = svclastb_n_f32 (p0, d0, z2),
		  d0 = svclastb (p0, d0, z2))

/*
** clastb_d1_f32:
** (
**	fmov	s0, s1
**	clastb	s0, p0, s0, z2\.s
** |
**	clastb	s1, p0, s1, z2\.s
**	fmov	s0, s1
** )
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d1_f32, float32_t, svfloat32_t,
		  d0 = svclastb_n_f32 (p0, d1, z2),
		  d0 = svclastb (p0, d1, z2))
