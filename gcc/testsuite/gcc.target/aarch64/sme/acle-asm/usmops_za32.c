/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** usmops_za32_u8_0_p0_p1_z0_z4:
**	usmops	za0\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (usmops_za32_u8_0_p0_p1_z0_z4, svuint8_t, svint8_t,
	      svusmops_za32_u8_m (0, p0, p1, z0, z4),
	      svusmops_za32_m (0, p0, p1, z0, z4))

/*
** usmops_za32_u8_0_p1_p0_z4_z0:
**	usmops	za0\.s, p1/m, p0/m, z4\.b, z0\.b
**	ret
*/
TEST_DUAL_ZA (usmops_za32_u8_0_p1_p0_z4_z0, svint8_t, svuint8_t,
	      svusmops_za32_u8_m (0, p1, p0, z4, z0),
	      svusmops_za32_m (0, p1, p0, z4, z0))

/*
** usmops_za32_u8_3_p0_p1_z0_z4:
**	usmops	za3\.s, p0/m, p1/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_ZA (usmops_za32_u8_3_p0_p1_z0_z4, svuint8_t, svint8_t,
	      svusmops_za32_u8_m (3, p0, p1, z0, z4),
	      svusmops_za32_m (3, p0, p1, z0, z4))
