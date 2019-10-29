/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_h4_f16_tied1:
**	insr	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (insr_h4_f16_tied1, svfloat16_t, __fp16,
		 z0 = svinsr_n_f16 (z0, d4),
		 z0 = svinsr (z0, d4))

/*
** insr_h4_f16_untied:
**	movprfx	z0, z1
**	insr	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (insr_h4_f16_untied, svfloat16_t, __fp16,
		 z0 = svinsr_n_f16 (z1, d4),
		 z0 = svinsr (z1, d4))

/*
** insr_0_f16_tied1:
**	insr	z0\.h, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_f16_tied1, svfloat16_t,
		z0 = svinsr_n_f16 (z0, 0),
		z0 = svinsr (z0, 0))

/*
** insr_0_f16_untied:
**	movprfx	z0, z1
**	insr	z0\.h, wzr
**	ret
*/
TEST_UNIFORM_Z (insr_0_f16_untied, svfloat16_t,
		z0 = svinsr_n_f16 (z1, 0),
		z0 = svinsr (z1, 0))

/*
** insr_1_f16:
**	fmov	(h[0-9]+), #?1\.0(?:e\+0)?
**	insr	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (insr_1_f16, svfloat16_t,
		z0 = svinsr_n_f16 (z0, 1),
		z0 = svinsr (z0, 1))
