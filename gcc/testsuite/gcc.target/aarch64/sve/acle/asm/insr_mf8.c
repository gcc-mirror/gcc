/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** insr_w0_mf8_tied1:
**	insr	z0\.b, b4
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_mf8_tied1, svmfloat8_t, mfloat8_t,
		 z0 = svinsr_n_mf8 (z0, x0),
		 z0 = svinsr (z0, x0))

/*
** insr_w0_mf8_untied:
**	movprfx	z0, z1
**	insr	z0\.b, b4
**	ret
*/
TEST_UNIFORM_ZX (insr_w0_mf8_untied, svmfloat8_t, mfloat8_t,
		 z0 = svinsr_n_mf8 (z1, x0),
		 z0 = svinsr (z1, x0))
