/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** bmopa_za32_u32_0_p0_p1_z0_z1:
**	bmopa	za0\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (bmopa_za32_u32_0_p0_p1_z0_z1, svuint32_t,
		 svbmopa_za32_u32_m (0, p0, p1, z0, z1),
		 svbmopa_za32_m (0, p0, p1, z0, z1))

/*
** bmopa_za32_u32_0_p1_p0_z1_z0:
**	bmopa	za0\.s, p1/m, p0/m, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (bmopa_za32_u32_0_p1_p0_z1_z0, svuint32_t,
		 svbmopa_za32_u32_m (0, p1, p0, z1, z0),
		 svbmopa_za32_m (0, p1, p0, z1, z0))

/*
** bmopa_za32_u32_3_p0_p1_z0_z1:
**	bmopa	za3\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (bmopa_za32_u32_3_p0_p1_z0_z1, svuint32_t,
		 svbmopa_za32_u32_m (3, p0, p1, z0, z1),
		 svbmopa_za32_m (3, p0, p1, z0, z1))
