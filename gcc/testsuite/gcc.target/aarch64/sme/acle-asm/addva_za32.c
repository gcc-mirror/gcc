/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** addva_za32_s32_0_p0_p1_z0:
**	addva	za0\.s, p0/m, p1/m, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (addva_za32_s32_0_p0_p1_z0, svint32_t,
		 svaddva_za32_s32_m (0, p0, p1, z0),
		 svaddva_za32_m (0, p0, p1, z0))

/*
** addva_za32_s32_0_p1_p0_z1:
**	addva	za0\.s, p1/m, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (addva_za32_s32_0_p1_p0_z1, svint32_t,
		 svaddva_za32_s32_m (0, p1, p0, z1),
		 svaddva_za32_m (0, p1, p0, z1))

/*
** addva_za32_s32_1_p0_p1_z0:
**	addva	za1\.s, p0/m, p1/m, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (addva_za32_s32_1_p0_p1_z0, svint32_t,
		 svaddva_za32_s32_m (1, p0, p1, z0),
		 svaddva_za32_m (1, p0, p1, z0))

/*
** addva_za32_s32_3_p0_p1_z0:
**	addva	za3\.s, p0/m, p1/m, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (addva_za32_s32_3_p0_p1_z0, svint32_t,
		 svaddva_za32_s32_m (3, p0, p1, z0),
		 svaddva_za32_m (3, p0, p1, z0))

/*
** addva_za32_u32_0_p0_p1_z0:
**	addva	za0\.s, p0/m, p1/m, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (addva_za32_u32_0_p0_p1_z0, svuint32_t,
		 svaddva_za32_u32_m (0, p0, p1, z0),
		 svaddva_za32_m (0, p0, p1, z0))
