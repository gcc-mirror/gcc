/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** mopa_za32_s8_0_p0_p1_z0_z1:
**	smopa	za0\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_s8_0_p0_p1_z0_z1, svint8_t,
		 svmopa_za32_s8_m (0, p0, p1, z0, z1),
		 svmopa_za32_m (0, p0, p1, z0, z1))

/*
** mopa_za32_s8_0_p1_p0_z1_z0:
**	smopa	za0\.s, p1/m, p0/m, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_s8_0_p1_p0_z1_z0, svint8_t,
		 svmopa_za32_s8_m (0, p1, p0, z1, z0),
		 svmopa_za32_m (0, p1, p0, z1, z0))

/*
** mopa_za32_s8_3_p0_p1_z0_z1:
**	smopa	za3\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_s8_3_p0_p1_z0_z1, svint8_t,
		 svmopa_za32_s8_m (3, p0, p1, z0, z1),
		 svmopa_za32_m (3, p0, p1, z0, z1))

/*
** mopa_za32_u8_0_p0_p1_z0_z1:
**	umopa	za0\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_u8_0_p0_p1_z0_z1, svuint8_t,
		 svmopa_za32_u8_m (0, p0, p1, z0, z1),
		 svmopa_za32_m (0, p0, p1, z0, z1))

/*
** mopa_za32_u8_3_p0_p1_z0_z1:
**	umopa	za3\.s, p0/m, p1/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_u8_3_p0_p1_z0_z1, svuint8_t,
		 svmopa_za32_u8_m (3, p0, p1, z0, z1),
		 svmopa_za32_m (3, p0, p1, z0, z1))

/*
** mopa_za32_bf16_0_p0_p1_z0_z1:
**	bfmopa	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_bf16_0_p0_p1_z0_z1, svbfloat16_t,
		 svmopa_za32_bf16_m (0, p0, p1, z0, z1),
		 svmopa_za32_m (0, p0, p1, z0, z1))

/*
** mopa_za32_bf16_3_p0_p1_z0_z1:
**	bfmopa	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_bf16_3_p0_p1_z0_z1, svbfloat16_t,
		 svmopa_za32_bf16_m (3, p0, p1, z0, z1),
		 svmopa_za32_m (3, p0, p1, z0, z1))

/*
** mopa_za32_f16_0_p0_p1_z0_z1:
**	fmopa	za0\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_f16_0_p0_p1_z0_z1, svfloat16_t,
		 svmopa_za32_f16_m (0, p0, p1, z0, z1),
		 svmopa_za32_m (0, p0, p1, z0, z1))

/*
** mopa_za32_f16_3_p0_p1_z0_z1:
**	fmopa	za3\.s, p0/m, p1/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_f16_3_p0_p1_z0_z1, svfloat16_t,
		 svmopa_za32_f16_m (3, p0, p1, z0, z1),
		 svmopa_za32_m (3, p0, p1, z0, z1))

/*
** mopa_za32_f32_0_p0_p1_z0_z1:
**	fmopa	za0\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_f32_0_p0_p1_z0_z1, svfloat32_t,
		 svmopa_za32_f32_m (0, p0, p1, z0, z1),
		 svmopa_za32_m (0, p0, p1, z0, z1))

/*
** mopa_za32_f32_3_p0_p1_z0_z1:
**	fmopa	za3\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (mopa_za32_f32_3_p0_p1_z0_z1, svfloat32_t,
		 svmopa_za32_f32_m (3, p0, p1, z0, z1),
		 svmopa_za32_m (3, p0, p1, z0, z1))
