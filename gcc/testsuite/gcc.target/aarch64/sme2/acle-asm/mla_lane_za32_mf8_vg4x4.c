/* { dg-do assemble { target { aarch64_asm_sme-f8f32_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f32_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme-f8f32"

/*
** mla_lane_0_z0_z4_0:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (mla_lane_0_z0_z4_0, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (0, z0, z4, 0, fpm0),
	      svmla_lane_za32_vg4x4_fpm (0, z0, z4, 0, fpm0))

/*
** mla_lane_w0_z0_z7_1:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z7\.b\[1\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0_z0_z7_1, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w0, z0, z7, 1, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w0, z0, z7, 1, fpm0))

/*
** mla_lane_w8_z28_z4_2:
** 	msr	fpmr, x1
**	fmlall	za\.s\[w8, 0:3, vgx4\], {z28\.b - z31\.b}, z4\.b\[2\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z4_2, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8, z28, z4, 2, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8, z28, z4, 2, fpm0))

/*
** mla_lane_w11p4_z0_z4_7:
** 	msr	fpmr, x1
**	fmlall	za\.s\[w11, 4:7, vgx4\], {z0\.b - z3\.b}, z4\.b\[7\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w11p4_z0_z4_7, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w11 + 4, z0, z4, 7, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w11 + 4, z0, z4, 7, fpm0))

/*
** mla_lane_w8p6_z0_z4_8:
**	add	(w8|w9|w10|w11), w8, #?6
** 	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[8\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p6_z0_z4_8, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8 + 6, z0, z4, 8, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8 + 6, z0, z4, 8, fpm0))

/*
** mla_lane_w8p7_z0_z4_9:
**	add	(w8|w9|w10|w11), w8, #?7
** 	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[9\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p7_z0_z4_9, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8 + 7, z0, z4, 9, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8 + 7, z0, z4, 9, fpm0))

/*
** mla_lane_w8p8_z0_z4_10:
**	add	(w8|w9|w10|w11), w8, #?8
** 	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[10\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8p8_z0_z4_10, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8 + 8, z0, z4, 10, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8 + 8, z0, z4, 10, fpm0))

/*
** mla_lane_w0m1_z0_z4_11:
**	sub	(w8|w9|w10|w11), w0, #?1
** 	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, z4\.b\[11\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w0m1_z0_z4_11, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w0 - 1, z0, z4, 11, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w0 - 1, z0, z4, 11, fpm0))

/*
** mla_lane_w8_z4_z15_12:
**	str	d15, \[sp, #?-16\]!
** 	msr	fpmr, x1
**	fmlall	za\.s\[w8, 0:3, vgx4\], {z4\.b - z7\.b}, z15\.b\[12\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (mla_lane_w8_z4_z15_12, svmfloat8x4_t, svmfloat8_t,
		  svmla_lane_za32_mf8_vg4x4_fpm (w8, z4, z15, 12, fpm0),
		  svmla_lane_za32_vg4x4_fpm (w8, z4, z15, 12, fpm0))

/*
** mla_lane_w8_z28_z16_13:
** 	msr	fpmr, x1
**	mov	(z[0-7]).d, z16.d
**	fmlall	za\.s\[w8, 0:3, vgx4\], {z28\.b - z31\.b}, \1\.b\[13\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z28_z16_13, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8, z28, z16, 13, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8, z28, z16, 13, fpm0))

/*
** mla_lane_w8_z17_z7_14:
** 	msr	fpmr, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmlall	za\.s\[w8, 0:3, vgx4\], [^\n]+, z7\.b\[14\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z17_z7_14, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8, z17, z7, 14, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8, z17, z7, 14, fpm0))

/*
** mla_lane_w8_z22_z4_15:
** 	msr	fpmr, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fmlall	za\.s\[w8, 0:3, vgx4\], [^\n]+, z4\.b\[15\]
**	ret
*/
TEST_ZA_LANE (mla_lane_w8_z22_z4_15, svmfloat8x4_t, svmfloat8_t,
	      svmla_lane_za32_mf8_vg4x4_fpm (w8, z22, z4, 15, fpm0),
	      svmla_lane_za32_vg4x4_fpm (w8, z22, z4, 15, fpm0))
