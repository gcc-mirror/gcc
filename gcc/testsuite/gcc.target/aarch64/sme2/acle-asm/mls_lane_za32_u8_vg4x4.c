/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (mls_lane_0_z0_z4_0, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (0, z0, z4, 0),
	      svmls_lane_za32_vg4x4 (0, z0, z4, 0))

/*
** mls_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z7\.b\[1\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w0_z0_z7_1, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w0, z0, z7, 1),
	      svmls_lane_za32_vg4x4 (w0, z0, z7, 1))

/*
** mls_lane_w8_z28_z4_2:
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z28\.b - z31\.b}, z4\.b\[2\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z28_z4_2, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8, z28, z4, 2),
	      svmls_lane_za32_vg4x4 (w8, z28, z4, 2))

/*
** mls_lane_w11p4_z0_z4_7:
**	umlsll	za\.s\[w11, 4:7, vgx4\], {z0\.b - z3\.b}, z4\.b\[7\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w11p4_z0_z4_7, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w11 + 4, z0, z4, 7),
	      svmls_lane_za32_vg4x4 (w11 + 4, z0, z4, 7))

/*
** mls_lane_w8p6_z0_z4_8:
**	add	(w8|w9|w10|w11), w8, #?6
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[8\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8p6_z0_z4_8, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8 + 6, z0, z4, 8),
	      svmls_lane_za32_vg4x4 (w8 + 6, z0, z4, 8))

/*
** mls_lane_w8p7_z0_z4_9:
**	add	(w8|w9|w10|w11), w8, #?7
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[9\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8p7_z0_z4_9, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8 + 7, z0, z4, 9),
	      svmls_lane_za32_vg4x4 (w8 + 7, z0, z4, 9))

/*
** mls_lane_w8p8_z0_z4_10:
**	add	(w8|w9|w10|w11), w8, #?8
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[10\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8p8_z0_z4_10, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8 + 8, z0, z4, 10),
	      svmls_lane_za32_vg4x4 (w8 + 8, z0, z4, 10))

/*
** mls_lane_w0m1_z0_z4_11:
**	sub	(w8|w9|w10|w11), w0, #?1
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[11\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w0m1_z0_z4_11, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w0 - 1, z0, z4, 11),
	      svmls_lane_za32_vg4x4 (w0 - 1, z0, z4, 11))

/*
** mls_lane_w8_z4_z15_12:
**	str	d15, \[sp, #?-16\]!
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z4\.b - z7\.b}, z15\.b\[12\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mls_lane_w8_z4_z15_12, svuint8x4_t, svuint8_t,
		  svmls_lane_za32_u8_vg4x4 (w8, z4, z15, 12),
		  svmls_lane_za32_vg4x4 (w8, z4, z15, 12))

/*
** mls_lane_w8_z28_z16_13:
**	mov	(z[0-7]).d, z16.d
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z28\.b - z31\.b}, \1\.b\[13\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z28_z16_13, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8, z28, z16, 13),
	      svmls_lane_za32_vg4x4 (w8, z28, z16, 13))

/*
** mls_lane_w8_z17_z7_14:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umlsll	za\.s\[w8, 0:3, vgx4\], [^\n]+, z7\.b\[14\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z17_z7_14, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8, z17, z7, 14),
	      svmls_lane_za32_vg4x4 (w8, z17, z7, 14))

/*
** mls_lane_w8_z22_z4_15:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	umlsll	za\.s\[w8, 0:3, vgx4\], [^\n]+, z4\.b\[15\]
**	ret
*/
TEST_ZA_LANE (mls_lane_w8_z22_z4_15, svuint8x4_t, svuint8_t,
	      svmls_lane_za32_u8_vg4x4 (w8, z22, z4, 15),
	      svmls_lane_za32_vg4x4 (w8, z22, z4, 15))
