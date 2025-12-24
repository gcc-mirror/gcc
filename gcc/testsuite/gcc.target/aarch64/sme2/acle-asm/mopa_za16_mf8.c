/* { dg-do assemble { target aarch64_asm_sme-f8f16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-f8f16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-f8f16"
/*
** mopa_za16_mf8_0_p0_p1_z0_z1:
** 	msr	fpmr, x0
**	fmopa	za0\.h, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_mf8_0_p0_p1_z0_z1, svmfloat8_t,
		 svmopa_za16_mf8_m_fpm (0, p0, p1, z0, z1, fpm0),
		 svmopa_za16_m_fpm (0, p0, p1, z0, z1, fpm0))

/*
** mopa_za16_mf8_0_p1_p0_z1_z0:
** 	msr	fpmr, x0
**	fmopa	za0\.h, p1/m, p0/m, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_mf8_0_p1_p0_z1_z0, svmfloat8_t,
		 svmopa_za16_mf8_m_fpm (0, p1, p0, z1, z0, fpm0),
		 svmopa_za16_m_fpm (0, p1, p0, z1, z0, fpm0))

/*
** mopa_za16_mf8_1_p0_p1_z0_z1:
** 	msr	fpmr, x0
**	fmopa	za1\.h, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za16_mf8_1_p0_p1_z0_z1, svmfloat8_t,
		 svmopa_za16_mf8_m_fpm (1, p0, p1, z0, z1, fpm0),
		 svmopa_za16_m_fpm (1, p0, p1, z0, z1, fpm0))
