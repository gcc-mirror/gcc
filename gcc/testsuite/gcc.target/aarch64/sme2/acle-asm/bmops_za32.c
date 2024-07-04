/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** bmops_za32_u32_0_p0_p1_z0_z1:
**	bmops	za0\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (bmops_za32_u32_0_p0_p1_z0_z1, svuint32_t,
		 svbmops_za32_u32_m (0, p0, p1, z0, z1),
		 svbmops_za32_m (0, p0, p1, z0, z1))

/*
** bmops_za32_u32_0_p1_p0_z1_z0:
**	bmops	za0\.s, p1/m, p0/m, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_ZA (bmops_za32_u32_0_p1_p0_z1_z0, svuint32_t,
		 svbmops_za32_u32_m (0, p1, p0, z1, z0),
		 svbmops_za32_m (0, p1, p0, z1, z0))

/*
** bmops_za32_u32_3_p0_p1_z0_z1:
**	bmops	za3\.s, p0/m, p1/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZA (bmops_za32_u32_3_p0_p1_z0_z1, svuint32_t,
		 svbmops_za32_u32_m (3, p0, p1, z0, z1),
		 svbmops_za32_m (3, p0, p1, z0, z1))
