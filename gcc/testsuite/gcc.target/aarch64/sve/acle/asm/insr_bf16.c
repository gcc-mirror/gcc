/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_h4_bf16_tied1:
**	insr	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (insr_h4_bf16_tied1, svbfloat16_t, bfloat16_t,
		 z0 = svinsr_n_bf16 (z0, d4),
		 z0 = svinsr (z0, d4))

/*
** insr_h4_bf16_untied:
**	movprfx	z0, z1
**	insr	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (insr_h4_bf16_untied, svbfloat16_t, bfloat16_t,
		 z0 = svinsr_n_bf16 (z1, d4),
		 z0 = svinsr (z1, d4))
