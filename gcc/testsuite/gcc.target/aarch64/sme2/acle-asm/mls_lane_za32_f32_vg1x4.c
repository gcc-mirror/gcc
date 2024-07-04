/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	fmls	za\.s\[\1, 0, vgx4\], {z0\.s - z3\.s}, z4\.s\[0\]
**	ret
*/
TEST_ZA_LANE (mls_lane_0_z0_z4_0, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (0, z0, z4, 0),
	      svmls_lane_za32_vg1x4 (0, z0, z4, 0))

/*
** mls_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	fmls	za\.s\[\1, 0, vgx4\], {z0\.s - z3\.s}, z7\.s\[1\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w0_z0_z7_1, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w0, z0, z7, 1),
	      svmls_lane_za32_vg1x4 (w0, z0, z7, 1))

/*
** mls_lane_w8_z28_z4_2:
**	fmls	za\.s\[w8, 0, vgx4\], {z28\.s - z31\.s}, z4\.s\[2\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z28_z4_2, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8, z28, z4, 2),
	      svmls_lane_za32_vg1x4 (w8, z28, z4, 2))

/*
** mls_lane_w8p7_z0_z4_3:
**	fmls	za\.s\[w8, 7, vgx4\], {z0\.s - z3\.s}, z4\.s\[3\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8p7_z0_z4_3, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8 + 7, z0, z4, 3),
	      svmls_lane_za32_vg1x4 (w8 + 7, z0, z4, 3))

/*
** mls_lane_w8p8_z0_z4_0:
**	add	(w8|w9|w10|w11), w8, #?8
**	fmls	za\.s\[\1, 0, vgx4\], {z0\.s - z3\.s}, z4\.s\[0\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8p8_z0_z4_0, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8 + 8, z0, z4, 0),
	      svmls_lane_za32_vg1x4 (w8 + 8, z0, z4, 0))

/*
** mls_lane_w0m1_z0_z4_1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	fmls	za\.s\[\1, 0, vgx4\], {z0\.s - z3\.s}, z4\.s\[1\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w0m1_z0_z4_1, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w0 - 1, z0, z4, 1),
	      svmls_lane_za32_vg1x4 (w0 - 1, z0, z4, 1))

/*
** mls_lane_w8_z4_z15_2:
**	str	d15, \[sp, #?-16\]!
**	fmls	za\.s\[w8, 0, vgx4\], {z4\.s - z7\.s}, z15\.s\[2\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mls_lane_w8_z4_z15_2, svfloat32x4_t, svfloat32_t,
		  svmls_lane_za32_f32_vg1x4 (w8, z4, z15, 2),
		  svmls_lane_za32_vg1x4 (w8, z4, z15, 2))

/*
** mls_lane_w8_z28_z16_3:
**	mov	(z[0-7]).d, z16.d
**	fmls	za\.s\[w8, 0, vgx4\], {z28\.s - z31\.s}, \1\.s\[3\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z28_z16_3, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8, z28, z16, 3),
	      svmls_lane_za32_vg1x4 (w8, z28, z16, 3))

/*
** mls_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmls	za\.s\[w8, 0, vgx4\], [^\n]+, z7\.s\[0\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z17_z7_0, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8, z17, z7, 0),
	      svmls_lane_za32_vg1x4 (w8, z17, z7, 0))

/*
** mls_lane_w8_z22_z4_1:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmls	za\.s\[w8, 0, vgx4\], [^\n]+, z4\.s\[1\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z22_z4_1, svfloat32x4_t, svfloat32_t,
	      svmls_lane_za32_f32_vg1x4 (w8, z22, z4, 1),
	      svmls_lane_za32_vg1x4 (w8, z22, z4, 1))
