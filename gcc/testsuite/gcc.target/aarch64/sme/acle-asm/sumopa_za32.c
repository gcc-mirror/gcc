/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** sumopa_za32_s8_0_p0_p1_z0_z4:
**	sumopa	za0\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (sumopa_za32_s8_0_p0_p1_z0_z4, svint8_t, svuint8_t,
	      svsumopa_za32_s8_m (0, p0, p1, z0, z4),
	      svsumopa_za32_m (0, p0, p1, z0, z4))

/*
** sumopa_za32_s8_0_p1_p0_z4_z0:
**	sumopa	za0\.s, p1/m, p0/m, z4\.b, z0\.b
**	ret
*/
TEST_DUAL_ZA (sumopa_za32_s8_0_p1_p0_z4_z0, svuint8_t, svint8_t,
	      svsumopa_za32_s8_m (0, p1, p0, z4, z0),
	      svsumopa_za32_m (0, p1, p0, z4, z0))

/*
** sumopa_za32_s8_3_p0_p1_z0_z4:
**	sumopa	za3\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (sumopa_za32_s8_3_p0_p1_z0_z4, svint8_t, svuint8_t,
	      svsumopa_za32_s8_m (3, p0, p1, z0, z4),
	      svsumopa_za32_m (3, p0, p1, z0, z4))
