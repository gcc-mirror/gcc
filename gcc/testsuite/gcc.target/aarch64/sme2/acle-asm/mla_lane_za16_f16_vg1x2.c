/* { dg-do assemble { target aarch64_asm_sme-f16f16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-f16f16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-f16f16"

/*
** mla_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	fmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, z4\.h\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_0_z0_z4_0, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (0, z0, z4, 0),
	      svmla_lane_za16_vg1x2 (0, z0, z4, 0))

/*
** mla_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	fmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, z7\.h\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0_z0_z7_1, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w0, z0, z7, 1),
	      svmla_lane_za16_vg1x2 (w0, z0, z7, 1))

/*
** mla_lane_w8_z28_z4_2:
**	fmla	za\.h\[w8, 0, vgx2\], {z28\.h - z29\.h}, z4\.h\[2\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z4_2, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8, z28, z4, 2),
	      svmla_lane_za16_vg1x2 (w8, z28, z4, 2))

/*
** mla_lane_w8p7_z0_z4_3:
**	fmla	za\.h\[w8, 7, vgx2\], {z0\.h - z1\.h}, z4\.h\[3\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p7_z0_z4_3, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8 + 7, z0, z4, 3),
	      svmla_lane_za16_vg1x2 (w8 + 7, z0, z4, 3))

/*
** mla_lane_w8p8_z0_z4_4:
**	add	(w8|w9|w10|w11), w8, #?8
**	fmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, z4\.h\[4\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p8_z0_z4_4, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8 + 8, z0, z4, 4),
	      svmla_lane_za16_vg1x2 (w8 + 8, z0, z4, 4))

/*
** mla_lane_w0m1_z0_z4_5:
**	sub	(w8|w9|w10|w11), w0, #?1
**	fmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, z4\.h\[5\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0m1_z0_z4_5, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w0 - 1, z0, z4, 5),
	      svmla_lane_za16_vg1x2 (w0 - 1, z0, z4, 5))

/*
** mla_lane_w8_z4_z15_6:
**	str	d15, \[sp, #?-16\]!
**	fmla	za\.h\[w8, 0, vgx2\], {z4\.h - z5\.h}, z15\.h\[6\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mla_lane_w8_z4_z15_6, svfloat16x2_t, svfloat16_t,
		  svmla_lane_za16_f16_vg1x2 (w8, z4, z15, 6),
		  svmla_lane_za16_vg1x2 (w8, z4, z15, 6))

/*
** mla_lane_w8_z28_z16_7:
**	mov	(z[0-7]).d, z16.d
**	fmla	za\.h\[w8, 0, vgx2\], {z28\.h - z29\.h}, \1\.h\[7\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z16_7, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8, z28, z16, 7),
	      svmla_lane_za16_vg1x2 (w8, z28, z16, 7))

/*
** mla_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	fmla	za\.h\[w8, 0, vgx2\], [^\n]+, z7\.h\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z17_z7_0, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8, z17, z7, 0),
	      svmla_lane_za16_vg1x2 (w8, z17, z7, 0))

/*
** mla_lane_w8_z22_z4_1:
**	fmla	za\.h\[w8, 0, vgx2\], {z22\.h - z23\.h}, z4\.h\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z22_z4_1, svfloat16x2_t, svfloat16_t,
	      svmla_lane_za16_f16_vg1x2 (w8, z22, z4, 1),
	      svmla_lane_za16_vg1x2 (w8, z22, z4, 1))
