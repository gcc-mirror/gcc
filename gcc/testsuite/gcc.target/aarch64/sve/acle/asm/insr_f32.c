/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_s4_f32_tied1:
**	insr	z0\.s, s4
**	ret
*/
TEST_UNIFORM_ZD (insr_s4_f32_tied1, svfloat32_t, float,
		 z0 = svinsr_n_f32 (z0, d4),
		 z0 = svinsr (z0, d4))

/*
** insr_s4_f32_untied:
**	movprfx	z0, z1
**	insr	z0\.s, s4
**	ret
*/
TEST_UNIFORM_ZD (insr_s4_f32_untied, svfloat32_t, float,
		 z0 = svinsr_n_f32 (z1, d4),
		 z0 = svinsr (z1, d4))

/*
** insr_0_f32_tied1:
**	insr	z0\.s, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_f32_tied1, svfloat32_t,
		z0 = svinsr_n_f32 (z0, 0),
		z0 = svinsr (z0, 0))

/*
** insr_0_f32_untied:
**	movprfx	z0, z1
**	insr	z0\.s, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_f32_untied, svfloat32_t,
		z0 = svinsr_n_f32 (z1, 0),
		z0 = svinsr (z1, 0))

/*
** insr_1_f32:
**	fmov	(s[0-9]+), #?1\.0(?:e\+0)?
**	insr	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (insr_1_f32, svfloat32_t,
		z0 = svinsr_n_f32 (z0, 1),
		z0 = svinsr (z0, 1))
