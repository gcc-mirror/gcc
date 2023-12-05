/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mops_za32_s16_0_p0_p1_z0_z1:
**	smops	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s16_0_p0_p1_z0_z1, svint16_t,
		 svmops_za32_s16_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_s16_0_p1_p0_z1_z0:
**	smops	za0\.s, p1/m, p0/m, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s16_0_p1_p0_z1_z0, svint16_t,
		 svmops_za32_s16_m (0, p1, p0, z1, z0),
		 svmops_za32_m (0, p1, p0, z1, z0))

/*
** mops_za32_s16_3_p0_p1_z0_z1:
**	smops	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s16_3_p0_p1_z0_z1, svint16_t,
		 svmops_za32_s16_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))

/*
** mops_za32_u16_0_p0_p1_z0_z1:
**	umops	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_u16_0_p0_p1_z0_z1, svuint16_t,
		 svmops_za32_u16_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_u16_3_p0_p1_z0_z1:
**	umops	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_u16_3_p0_p1_z0_z1, svuint16_t,
		 svmops_za32_u16_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))
