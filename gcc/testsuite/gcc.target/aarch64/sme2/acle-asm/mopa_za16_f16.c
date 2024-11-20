/* { dg-do assemble { target aarch64_asm_sme-f16f16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-f16f16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-f16f16"

/*
** mopa_za16_f16_0_p0_p1_z0_z1:
**	fmopa	za0\.h, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_f16_0_p0_p1_z0_z1, svfloat16_t,
		 svmopa_za16_f16_m (0, p0, p1, z0, z1),
		 svmopa_za16_m (0, p0, p1, z0, z1))

/*
** mopa_za16_f16_0_p1_p0_z1_z0:
**	fmopa	za0\.h, p1/m, p0/m, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_f16_0_p1_p0_z1_z0, svfloat16_t,
		 svmopa_za16_f16_m (0, p1, p0, z1, z0),
		 svmopa_za16_m (0, p1, p0, z1, z0))

/*
** mopa_za16_f16_1_p0_p1_z0_z1:
**	fmopa	za1\.h, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_f16_1_p0_p1_z0_z1, svfloat16_t,
		 svmopa_za16_f16_m (1, p0, p1, z0, z1),
		 svmopa_za16_m (1, p0, p1, z0, z1))
