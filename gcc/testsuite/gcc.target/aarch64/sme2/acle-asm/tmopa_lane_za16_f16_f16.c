/* { dg-do assemble { target { aarch64_asm_sme-f16f16_ok && aarch64_asm_sme-tmop_ok}  } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f16f16_ok && aarch64_asm_sme-tmop_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-tmop+sme-f16f16"

/*
** tmopa_lane_za16_f16_f16_0_z0_z4_z20_0:
**	ftmopa	za0\.h, {z0\.h - z1\.h}, z4\.h, z20\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z0_z4_z20_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z0, z4, z20, 0),
		 svtmopa_lane_za16 (0, z0, z4, z20, 0))

/* ZA slice and offset with different values.
** tmopa_lane_za16_f16_f16_1_z2_z4_z20_3:
**	ftmopa	za1\.h, {z2\.h - z3\.h}, z4\.h, z20\[3\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_1_z2_z4_z20_3, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (1, z2, z4, z20, 3),
		 svtmopa_lane_za16 (1, z2, z4, z20, 3))

/* The first register on the second argument must be even.
** tmopa_lane_za16_f16_f16_0_z1_z4_z20_0:
**	mov	(z\d+)\.d, z1\.d
**	mov	(z\d+)\.d, z2\.d
**	ftmopa	za0\.h, {\1\.h - \2\.h}, z4\.h, z20\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z1_z4_z20_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z1, z4, z20, 0),
		 svtmopa_lane_za16 (0, z1, z4, z20, 0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za16_f16_f16_0_z0_z4_z19_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z19.d
**	ftmopa	za0\.h, {z0\.h - z1\.h}, z4\.h, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z0_z4_z19_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z0, z4, z19, 0),
		 svtmopa_lane_za16 (0, z0, z4, z19, 0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za16_f16_f16_0_z0_z4_z24_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z24.d
**	ftmopa	za0\.h, {z0\.h - z1\.h}, z4\.h, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z0_z4_z24_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z0, z4, z24, 0),
		 svtmopa_lane_za16 (0, z0, z4, z24, 0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za16_f16_f16_0_z0_z4_z27_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z27.d
**	ftmopa	za0\.h, {z0\.h - z1\.h}, z4\.h, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z0_z4_z27_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z0, z4, z27, 0),
		 svtmopa_lane_za16 (0, z0, z4, z27, 0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za16_f16_f16_0_z0_z4_z28_0:
**	ftmopa	za0\.h, {z0\.h - z1\.h}, z4\.h, z28\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za16_f16_f16_0_z0_z4_z28_0, svfloat16x2_t, svfloat16_t,
		 svtmopa_lane_za16_f16_f16 (0, z0, z4, z28, 0),
		 svtmopa_lane_za16 (0, z0, z4, z28, 0))

