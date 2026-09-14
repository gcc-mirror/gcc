/* { dg-do assemble { target aarch64_asm_sme-tmop_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-tmop_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-tmop+sme-f8f32"

/*
** tmopa_lane_za32_mf8_mf8_0_z0_z4_z20_0:
**	msr	fpmr, x0
**	ftmopa	za0\.s, {z0\.b - z1\.b}, z4\.b, z20\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z0_z4_z20_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z0, z4, z20, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z0, z4, z20, 0, fpm0))

/* ZA slice and offset with maximum values.
** tmopa_lane_za32_mf8_mf8_3_z2_z4_z20_3:
**	msr	fpmr, x0
**	ftmopa	za3\.s, {z2\.b - z3\.b}, z4\.b, z20\[3\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_3_z2_z4_z20_3, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (3, z2, z4, z20, 3, fpm0),
		 svtmopa_lane_za32_fpm (3, z2, z4, z20, 3, fpm0))

/* The first register on the second argument must be even.
** tmopa_lane_za32_mf8_mf8_0_z1_z4_z20_0:
**	msr	fpmr, x0
**	mov	(z\d+)\.d, z1\.d
**	mov	(z\d+)\.d, z2\.d
**	ftmopa	za0\.s, {\1\.b - \2\.b}, z4\.b, z20\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z1_z4_z20_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z1, z4, z20, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z1, z4, z20, 0, fpm0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za32_mf8_mf8_0_z0_z4_z19_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z19.d
**	msr	fpmr, x0
**	ftmopa	za0\.s, {z0\.b - z1\.b}, z4\.b, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z0_z4_z19_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z0, z4, z19, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z0, z4, z19, 0, fpm0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za32_mf8_mf8_0_z0_z4_z24_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z24.d
**	msr	fpmr, x0
**	ftmopa	za0\.s, {z0\.b - z1\.b}, z4\.b, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z0_z4_z24_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z0, z4, z24, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z0, z4, z24, 0, fpm0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za32_mf8_mf8_0_z0_z4_z27_0:
**	mov	z(2[0-3]|2[89]|3[01]).d, z27.d
**	msr	fpmr, x0
**	ftmopa	za0\.s, {z0\.b - z1\.b}, z4\.b, z\1\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z0_z4_z27_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z0, z4, z27, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z0, z4, z27, 0, fpm0))

/* zk register must be one of Z20-Z23 or Z28-z31.
** tmopa_lane_za32_mf8_mf8_0_z0_z4_z28_0:
**	msr	fpmr, x0
**	ftmopa	za0\.s, {z0\.b - z1\.b}, z4\.b, z28\[0\]
**	ret
*/
TEST_ZA_TMOP (tmopa_lane_za32_mf8_mf8_0_z0_z4_z28_0, svmfloat8x2_t, svmfloat8_t,
		 svtmopa_lane_za32_mf8_mf8_fpm (0, z0, z4, z28, 0, fpm0),
		 svtmopa_lane_za32_fpm (0, z0, z4, z28, 0, fpm0))

