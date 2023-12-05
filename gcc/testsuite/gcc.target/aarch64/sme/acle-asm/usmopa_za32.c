/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** usmopa_za32_u8_0_p0_p1_z0_z4:
**	usmopa	za0\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (usmopa_za32_u8_0_p0_p1_z0_z4, svuint8_t, svint8_t,
	      svusmopa_za32_u8_m (0, p0, p1, z0, z4),
	      svusmopa_za32_m (0, p0, p1, z0, z4))

/*
** usmopa_za32_u8_0_p1_p0_z4_z0:
**	usmopa	za0\.s, p1/m, p0/m, z4\.b, z0\.b
**	ret
*/
TEST_DUAL_ZA (usmopa_za32_u8_0_p1_p0_z4_z0, svint8_t, svuint8_t,
	      svusmopa_za32_u8_m (0, p1, p0, z4, z0),
	      svusmopa_za32_m (0, p1, p0, z4, z0))

/*
** usmopa_za32_u8_3_p0_p1_z0_z4:
**	usmopa	za3\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (usmopa_za32_u8_3_p0_p1_z0_z4, svuint8_t, svint8_t,
	      svusmopa_za32_u8_m (3, p0, p1, z0, z4),
	      svusmopa_za32_m (3, p0, p1, z0, z4))
