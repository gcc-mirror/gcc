/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** vdot_lane_0_z0_z4_0:
**	mov	(w8|w9|w10|w11), #?0
**	svdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, z4\.h\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_0_z0_z4_0, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (0, z0, z4, 0),
	      svvdot_lane_za64_vg1x4 (0, z0, z4, 0))

/*
** vdot_lane_w0_z0_z7_1:
**	mov	(w8|w9|w10|w11), w0
**	svdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, z7\.h\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w0_z0_z7_1, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w0, z0, z7, 1),
	      svvdot_lane_za64_vg1x4 (w0, z0, z7, 1))

/*
** vdot_lane_w8_z28_z4_0:
**	svdot	za\.d\[w8, 0, vgx4\], {z28\.h - z31\.h}, z4\.h\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z28_z4_0, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8, z28, z4, 0),
	      svvdot_lane_za64_vg1x4 (w8, z28, z4, 0))

/*
** vdot_lane_w8p7_z0_z4_1:
**	svdot	za\.d\[w8, 7, vgx4\], {z0\.h - z3\.h}, z4\.h\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8p7_z0_z4_1, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8 + 7, z0, z4, 1),
	      svvdot_lane_za64_vg1x4 (w8 + 7, z0, z4, 1))

/*
** vdot_lane_w8p8_z0_z4_0:
**	add	(w8|w9|w10|w11), w8, #?8
**	svdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, z4\.h\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8p8_z0_z4_0, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8 + 8, z0, z4, 0),
	      svvdot_lane_za64_vg1x4 (w8 + 8, z0, z4, 0))

/*
** vdot_lane_w0m1_z0_z4_1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	svdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, z4\.h\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w0m1_z0_z4_1, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w0 - 1, z0, z4, 1),
	      svvdot_lane_za64_vg1x4 (w0 - 1, z0, z4, 1))

/*
** vdot_lane_w8_z4_z15_0:
**	str	d15, \[sp, #?-16\]!
**	svdot	za\.d\[w8, 0, vgx4\], {z4\.h - z7\.h}, z15\.h\[0\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (vdot_lane_w8_z4_z15_0, svint16x4_t, svint16_t,
		  svvdot_lane_za64_s16_vg1x4 (w8, z4, z15, 0),
		  svvdot_lane_za64_vg1x4 (w8, z4, z15, 0))

/*
** vdot_lane_w8_z28_z16_1:
**	mov	(z[0-7]).d, z16.d
**	svdot	za\.d\[w8, 0, vgx4\], {z28\.h - z31\.h}, \1\.h\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z28_z16_1, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8, z28, z16, 1),
	      svvdot_lane_za64_vg1x4 (w8, z28, z16, 1))

/*
** vdot_lane_w8_z17_z7_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	svdot	za\.d\[w8, 0, vgx4\], [^\n]+, z7\.h\[0\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z17_z7_0, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8, z17, z7, 0),
	      svvdot_lane_za64_vg1x4 (w8, z17, z7, 0))

/*
** vdot_lane_w8_z22_z4_1:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	svdot	za\.d\[w8, 0, vgx4\], [^\n]+, z4\.h\[1\]
**	ret
*/
TEST_ZA_LANE (vdot_lane_w8_z22_z4_1, svint16x4_t, svint16_t,
	      svvdot_lane_za64_s16_vg1x4 (w8, z22, z4, 1),
	      svvdot_lane_za64_vg1x4 (w8, z22, z4, 1))
