/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_w0_mf8:
**	mov	z0\.b, b4
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_mf8, svmfloat8_t, mfloat8_t,
		 z0 = svdup_n_mf8 (x0),
		 z0 = svdup_mf8 (x0))

/*
** dup_w0_mf8_m:
**	movprfx	z0, z1
**	mov	z0\.b, p0/m, b4
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_mf8_m, svmfloat8_t, mfloat8_t,
		z0 = svdup_n_mf8_m (z1, p0, x0),
		z0 = svdup_mf8_m (z1, p0, x0))

/*
** dup_w0_mf8_x:
**	mov	z0\.b, b4
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_mf8_x, svmfloat8_t, mfloat8_t,
		z0 = svdup_n_mf8_x (p0, x0),
		z0 = svdup_mf8_x (p0, x0))
