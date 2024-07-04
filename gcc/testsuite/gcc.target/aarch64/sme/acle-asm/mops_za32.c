/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** mops_za32_s8_0_p0_p1_z0_z1:
**	smops	za0\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s8_0_p0_p1_z0_z1, svint8_t,
		 svmops_za32_s8_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_s8_0_p1_p0_z1_z0:
**	smops	za0\.s, p1/m, p0/m, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s8_0_p1_p0_z1_z0, svint8_t,
		 svmops_za32_s8_m (0, p1, p0, z1, z0),
		 svmops_za32_m (0, p1, p0, z1, z0))

/*
** mops_za32_s8_3_p0_p1_z0_z1:
**	smops	za3\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_s8_3_p0_p1_z0_z1, svint8_t,
		 svmops_za32_s8_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))

/*
** mops_za32_u8_0_p0_p1_z0_z1:
**	umops	za0\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_u8_0_p0_p1_z0_z1, svuint8_t,
		 svmops_za32_u8_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_u8_3_p0_p1_z0_z1:
**	umops	za3\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_u8_3_p0_p1_z0_z1, svuint8_t,
		 svmops_za32_u8_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))

/*
** mops_za32_bf16_0_p0_p1_z0_z1:
**	bfmops	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_bf16_0_p0_p1_z0_z1, svbfloat16_t,
		 svmops_za32_bf16_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_bf16_3_p0_p1_z0_z1:
**	bfmops	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_bf16_3_p0_p1_z0_z1, svbfloat16_t,
		 svmops_za32_bf16_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))

/*
** mops_za32_f16_0_p0_p1_z0_z1:
**	fmops	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_f16_0_p0_p1_z0_z1, svfloat16_t,
		 svmops_za32_f16_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_f16_3_p0_p1_z0_z1:
**	fmops	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_f16_3_p0_p1_z0_z1, svfloat16_t,
		 svmops_za32_f16_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))

/*
** mops_za32_f32_0_p0_p1_z0_z1:
**	fmops	za0\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_f32_0_p0_p1_z0_z1, svfloat32_t,
		 svmops_za32_f32_m (0, p0, p1, z0, z1),
		 svmops_za32_m (0, p0, p1, z0, z1))

/*
** mops_za32_f32_3_p0_p1_z0_z1:
**	fmops	za3\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (mops_za32_f32_3_p0_p1_z0_z1, svfloat32_t,
		 svmops_za32_f32_m (3, p0, p1, z0, z1),
		 svmops_za32_m (3, p0, p1, z0, z1))
