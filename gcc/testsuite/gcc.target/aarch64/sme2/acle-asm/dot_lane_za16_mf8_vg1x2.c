/* { dg-do assemble { target { aarch64_asm_sme-f8f16_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f16_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sve2,+sme-f8f16"

/* Available variants are:
   _za16 if __ARM_FEATURE_SME_F8F16 != 0
   void svdot_lane_za16[_mf8]_vg1x2_fpm (uint32_t slice, svmfloat8x2_t zn, svmfloat8_t zm, uint64_t imm_idx, fpm_t fpm) __arm_streaming __arm_inout("za");  */

/*
** dot_lane_0_z0_z4_0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (dot_lane_0_z0_z4_0, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (0, z0, z4, 0, fpm0),
	      svdot_lane_za16_vg1x2_fpm (0, z0, z4, 0, fpm0))

/*
** dot_lane_w0_z0_z7_1:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, z7\.b\[1\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w0_z0_z7_1, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w0, z0, z7, 1, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w0, z0, z7, 1, fpm0))

/*
** dot_lane_w8_z28_z4_2:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z28\.b - z29\.b}, z4\.b\[2\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8_z28_z4_2, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8, z28, z4, 2, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8, z28, z4, 2, fpm0))

/*
** dot_lane_w8p7_z0_z4_3:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 7, vgx2\], {z0\.b - z1\.b}, z4\.b\[3\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8p7_z0_z4_3, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8 + 7, z0, z4, 3, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8 + 7, z0, z4, 3, fpm0))

/*
** dot_lane_w8p8_z0_z4_0:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, z4\.b\[0\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8p8_z0_z4_0, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8 + 8, z0, z4, 0, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8 + 8, z0, z4, 0, fpm0))

/*
** dot_lane_w0m1_z0_z4_1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	msr	fpmr, x1
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, z4\.b\[1\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w0m1_z0_z4_1, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w0 - 1, z0, z4, 1, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w0 - 1, z0, z4, 1, fpm0))

/*
** dot_lane_w8_z4_z15_2:
**	str	d15, \[sp, #?-16\]!
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z4\.b - z5\.b}, z15\.b\[2\]
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_LANE_Z15 (dot_lane_w8_z4_z15_2, svmfloat8x2_t, svmfloat8_t,
		  svdot_lane_za16_mf8_vg1x2_fpm (w8, z4, z15, 2, fpm0),
		  svdot_lane_za16_vg1x2_fpm (w8, z4, z15, 2, fpm0))

/*
** dot_lane_w8_z28_z16_3:
**	msr	fpmr, x1
**	mov	(z[0-7]).d, z16.d
**	fdot	za\.h\[w8, 0, vgx2\], {z28\.b - z29\.b}, \1\.b\[3\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8_z28_z16_3, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8, z28, z16, 3, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8, z28, z16, 3, fpm0))

/*
** dot_lane_w8_z17_z7_0:
**	msr	fpmr, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	fdot	za\.h\[w8, 0, vgx2\], [^\n]+, z7\.b\[0\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8_z17_z7_0, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8, z17, z7, 0, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8, z17, z7, 0, fpm0))

/*
** dot_lane_w8_z22_z4_1:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z22\.b - z23\.b}, z4\.b\[1\]
**	ret
*/
TEST_ZA_LANE (dot_lane_w8_z22_z4_1, svmfloat8x2_t, svmfloat8_t,
	      svdot_lane_za16_mf8_vg1x2_fpm (w8, z22, z4, 1, fpm0),
	      svdot_lane_za16_vg1x2_fpm (w8, z22, z4, 1, fpm0))
