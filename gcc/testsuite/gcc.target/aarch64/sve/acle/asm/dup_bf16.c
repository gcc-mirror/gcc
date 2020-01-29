/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_h4_bf16:
**	mov	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_bf16, svbfloat16_t, __bf16,
		z0 = svdup_n_bf16 (d4),
		z0 = svdup_bf16 (d4))

/*
** dup_h4_bf16_m:
**	movprfx	z0, z1
**	mov	z0\.h, p0/m, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_bf16_m, svbfloat16_t, __bf16,
		z0 = svdup_n_bf16_m (z1, p0, d4),
		z0 = svdup_bf16_m (z1, p0, d4))

/*
** dup_h4_bf16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	mov	z0\.h, p0/m, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_bf16_z, svbfloat16_t, __bf16,
		z0 = svdup_n_bf16_z (p0, d4),
		z0 = svdup_bf16_z (p0, d4))

/*
** dup_h4_bf16_x:
**	mov	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_bf16_x, svbfloat16_t, __bf16,
		z0 = svdup_n_bf16_x (p0, d4),
		z0 = svdup_bf16_x (p0, d4))
