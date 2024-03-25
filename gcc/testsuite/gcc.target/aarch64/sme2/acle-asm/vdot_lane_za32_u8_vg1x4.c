/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** vdot_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	uvdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_0_z0_z4_0, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (0, z0, z4, 0),
	      svvdot_lane_za32_vg1x4 (0, z0, z4, 0))

/*
** vdot_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	uvdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, z7\.b\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w0_z0_z7_1, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w0, z0, z7, 1),
	      svvdot_lane_za32_vg1x4 (w0, z0, z7, 1))

/*
** vdot_lane_w8_z28_z4_2:
**	uvdot	za\.s\[w8, 0, vgx4\], {z28\.b - z31\.b}, z4\.b\[2\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z28_z4_2, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8, z28, z4, 2),
	      svvdot_lane_za32_vg1x4 (w8, z28, z4, 2))

/*
** vdot_lane_w8p7_z0_z4_3:
**	uvdot	za\.s\[w8, 7, vgx4\], {z0\.b - z3\.b}, z4\.b\[3\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8p7_z0_z4_3, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8 + 7, z0, z4, 3),
	      svvdot_lane_za32_vg1x4 (w8 + 7, z0, z4, 3))

/*
** vdot_lane_w8p8_z0_z4_0:
**	add	(w8|w9|w10|w11), w8, #?8
**	uvdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8p8_z0_z4_0, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8 + 8, z0, z4, 0),
	      svvdot_lane_za32_vg1x4 (w8 + 8, z0, z4, 0))

/*
** vdot_lane_w0m1_z0_z4_1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	uvdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, z4\.b\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w0m1_z0_z4_1, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w0 - 1, z0, z4, 1),
	      svvdot_lane_za32_vg1x4 (w0 - 1, z0, z4, 1))

/*
** vdot_lane_w8_z4_z15_2:
**	str	d15, \[sp, #?-16\]!
**	uvdot	za\.s\[w8, 0, vgx4\], {z4\.b - z7\.b}, z15\.b\[2\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (vdot_lane_w8_z4_z15_2, svuint8x4_t, svuint8_t,
		  svvdot_lane_za32_u8_vg1x4 (w8, z4, z15, 2),
		  svvdot_lane_za32_vg1x4 (w8, z4, z15, 2))

/*
** vdot_lane_w8_z28_z16_3:
**	mov	(z[0-7]).d, z16.d
**	uvdot	za\.s\[w8, 0, vgx4\], {z28\.b - z31\.b}, \1\.b\[3\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z28_z16_3, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8, z28, z16, 3),
	      svvdot_lane_za32_vg1x4 (w8, z28, z16, 3))

/*
** vdot_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uvdot	za\.s\[w8, 0, vgx4\], [^\n]+, z7\.b\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z17_z7_0, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8, z17, z7, 0),
	      svvdot_lane_za32_vg1x4 (w8, z17, z7, 0))

/*
** vdot_lane_w8_z22_z4_1:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	uvdot	za\.s\[w8, 0, vgx4\], [^\n]+, z4\.b\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z22_z4_1, svuint8x4_t, svuint8_t,
	      svvdot_lane_za32_u8_vg1x4 (w8, z22, z4, 1),
	      svvdot_lane_za32_vg1x4 (w8, z22, z4, 1))
