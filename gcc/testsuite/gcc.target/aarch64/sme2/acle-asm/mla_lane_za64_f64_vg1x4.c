/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-f64f64"

#include "test_sme2_acle.h"

/*
** mla_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	fmla	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}, z4\.d\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_0_z0_z4_0, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (0, z0, z4, 0),
	      svmla_lane_za64_vg1x4 (0, z0, z4, 0))

/*
** mla_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	fmla	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}, z7\.d\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0_z0_z7_1, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w0, z0, z7, 1),
	      svmla_lane_za64_vg1x4 (w0, z0, z7, 1))

/*
** mla_lane_w8_z28_z4_2:
**	fmla	za\.d\[w8, 0, vgx4\], {z28\.d - z31\.d}, z4\.d\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z4_2, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8, z28, z4, 0),
	      svmla_lane_za64_vg1x4 (w8, z28, z4, 0))

/*
** mla_lane_w8p7_z0_z4_3:
**	fmla	za\.d\[w8, 7, vgx4\], {z0\.d - z3\.d}, z4\.d\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p7_z0_z4_3, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8 + 7, z0, z4, 1),
	      svmla_lane_za64_vg1x4 (w8 + 7, z0, z4, 1))

/*
** mla_lane_w8p8_z0_z4_0:
**	add	(w8|w9|w10|w11), w8, #?8
**	fmla	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}, z4\.d\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p8_z0_z4_0, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8 + 8, z0, z4, 0),
	      svmla_lane_za64_vg1x4 (w8 + 8, z0, z4, 0))

/*
** mla_lane_w0m1_z0_z4_1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	fmla	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}, z4\.d\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0m1_z0_z4_1, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w0 - 1, z0, z4, 1),
	      svmla_lane_za64_vg1x4 (w0 - 1, z0, z4, 1))

/*
** mla_lane_w8_z4_z15_2:
**	str	d15, \[sp, #?-16\]!
**	fmla	za\.d\[w8, 0, vgx4\], {z4\.d - z7\.d}, z15\.d\[0\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mla_lane_w8_z4_z15_2, svfloat64x4_t, svfloat64_t,
		  svmla_lane_za64_f64_vg1x4 (w8, z4, z15, 0),
		  svmla_lane_za64_vg1x4 (w8, z4, z15, 0))

/*
** mla_lane_w8_z28_z16_3:
**	mov	(z[0-7]).d, z16.d
**	fmla	za\.d\[w8, 0, vgx4\], {z28\.d - z31\.d}, \1\.d\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z16_3, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8, z28, z16, 1),
	      svmla_lane_za64_vg1x4 (w8, z28, z16, 1))

/*
** mla_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmla	za\.d\[w8, 0, vgx4\], [^\n]+, z7\.d\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z17_z7_0, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8, z17, z7, 0),
	      svmla_lane_za64_vg1x4 (w8, z17, z7, 0))

/*
** mla_lane_w8_z22_z4_1:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmla	za\.d\[w8, 0, vgx4\], [^\n]+, z4\.d\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z22_z4_1, svfloat64x4_t, svfloat64_t,
	      svmla_lane_za64_f64_vg1x4 (w8, z22, z4, 1),
	      svmla_lane_za64_vg1x4 (w8, z22, z4, 1))
