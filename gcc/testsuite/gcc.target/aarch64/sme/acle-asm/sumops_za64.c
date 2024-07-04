/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

#pragma GCC target "+sme-i16i64"

/*
** sumops_za64_s16_0_p0_p1_z0_z4:
**	sumops	za0\.d, p0/m, p1/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_ZA (sumops_za64_s16_0_p0_p1_z0_z4, svint16_t, svuint16_t,
	      svsumops_za64_s16_m (0, p0, p1, z0, z4),
	      svsumops_za64_m (0, p0, p1, z0, z4))

/*
** sumops_za64_s16_0_p1_p0_z4_z0:
**	sumops	za0\.d, p1/m, p0/m, z4\.h, z0\.h
**	ret
*/
TEST_DUAL_ZA (sumops_za64_s16_0_p1_p0_z4_z0, svuint16_t, svint16_t,
	      svsumops_za64_s16_m (0, p1, p0, z4, z0),
	      svsumops_za64_m (0, p1, p0, z4, z0))

/*
** sumops_za64_s16_7_p0_p1_z0_z4:
**	sumops	za7\.d, p0/m, p1/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_ZA (sumops_za64_s16_7_p0_p1_z0_z4, svint16_t, svuint16_t,
	      svsumops_za64_s16_m (7, p0, p1, z0, z4),
	      svsumops_za64_m (7, p0, p1, z0, z4))
