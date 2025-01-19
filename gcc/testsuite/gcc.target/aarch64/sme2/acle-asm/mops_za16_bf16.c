/* { dg-do assemble { target aarch64_asm_sme-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-b16b16"

/*
** mops_za16_bf16_0_p0_p1_z0_z1:
**	bfmops	za0\.h, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za16_bf16_0_p0_p1_z0_z1, svbfloat16_t,
		 svmops_za16_bf16_m (0, p0, p1, z0, z1),
		 svmops_za16_m (0, p0, p1, z0, z1))

/*
** mops_za16_bf16_0_p1_p0_z1_z0:
**	bfmops	za0\.h, p1/m, p0/m, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za16_bf16_0_p1_p0_z1_z0, svbfloat16_t,
		 svmops_za16_bf16_m (0, p1, p0, z1, z0),
		 svmops_za16_m (0, p1, p0, z1, z0))

/*
** mops_za16_bf16_1_p0_p1_z0_z1:
**	bfmops	za1\.h, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za16_bf16_1_p0_p1_z0_z1, svbfloat16_t,
		 svmops_za16_bf16_m (1, p0, p1, z0, z1),
		 svmops_za16_m (1, p0, p1, z0, z1))
