/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mla_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	bfmlal	za\.s\[\1, 0:1, vgx2\], {z0\.h - z1\.h}, z4\.h\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_0_z0_z4_0, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (0, z0, z4, 0),
	      svmla_lane_za32_vg2x2 (0, z0, z4, 0))

/*
** mla_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	bfmlal	za\.s\[\1, 0:1, vgx2\], {z0\.h - z1\.h}, z7\.h\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0_z0_z7_1, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w0, z0, z7, 1),
	      svmla_lane_za32_vg2x2 (w0, z0, z7, 1))

/*
** mla_lane_w8_z28_z4_2:
**	bfmlal	za\.s\[w8, 0:1, vgx2\], {z28\.h - z29\.h}, z4\.h\[2\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z4_2, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8, z28, z4, 2),
	      svmla_lane_za32_vg2x2 (w8, z28, z4, 2))

/*
** mla_lane_w8p6_z0_z4_7:
**	bfmlal	za\.s\[w8, 6:7, vgx2\], {z0\.h - z1\.h}, z4\.h\[7\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p6_z0_z4_7, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8 + 6, z0, z4, 7),
	      svmla_lane_za32_vg2x2 (w8 + 6, z0, z4, 7))

/*
** mla_lane_w8p7_z0_z4_3:
**	add	(w8|w9|w10|w11), w8, #?7
**	bfmlal	za\.s\[\1, 0:1, vgx2\], {z0\.h - z1\.h}, z4\.h\[3\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p7_z0_z4_3, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8 + 7, z0, z4, 3),
	      svmla_lane_za32_vg2x2 (w8 + 7, z0, z4, 3))

/*
** mla_lane_w8p8_z0_z4_4:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfmlal	za\.s\[\1, 0:1, vgx2\], {z0\.h - z1\.h}, z4\.h\[4\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p8_z0_z4_4, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8 + 8, z0, z4, 4),
	      svmla_lane_za32_vg2x2 (w8 + 8, z0, z4, 4))

/*
** mla_lane_w0m1_z0_z4_5:
**	sub	(w8|w9|w10|w11), w0, #?1
**	bfmlal	za\.s\[\1, 0:1, vgx2\], {z0\.h - z1\.h}, z4\.h\[5\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0m1_z0_z4_5, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w0 - 1, z0, z4, 5),
	      svmla_lane_za32_vg2x2 (w0 - 1, z0, z4, 5))

/*
** mla_lane_w8_z4_z15_6:
**	str	d15, \[sp, #?-16\]!
**	bfmlal	za\.s\[w8, 0:1, vgx2\], {z4\.h - z5\.h}, z15\.h\[6\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mla_lane_w8_z4_z15_6, svbfloat16x2_t, svbfloat16_t,
		  svmla_lane_za32_bf16_vg2x2 (w8, z4, z15, 6),
		  svmla_lane_za32_vg2x2 (w8, z4, z15, 6))

/*
** mla_lane_w8_z28_z16_7:
**	mov	(z[0-7]).d, z16.d
**	bfmlal	za\.s\[w8, 0:1, vgx2\], {z28\.h - z29\.h}, \1\.h\[7\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z16_7, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8, z28, z16, 7),
	      svmla_lane_za32_vg2x2 (w8, z28, z16, 7))

/*
** mla_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	bfmlal	za\.s\[w8, 0:1, vgx2\], [^\n]+, z7\.h\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z17_z7_0, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8, z17, z7, 0),
	      svmla_lane_za32_vg2x2 (w8, z17, z7, 0))

/*
** mla_lane_w8_z22_z4_1:
**	bfmlal	za\.s\[w8, 0:1, vgx2\], {z22\.h - z23\.h}, z4\.h\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z22_z4_1, svbfloat16x2_t, svbfloat16_t,
	      svmla_lane_za32_bf16_vg2x2 (w8, z22, z4, 1),
	      svmla_lane_za32_vg2x2 (w8, z22, z4, 1))
