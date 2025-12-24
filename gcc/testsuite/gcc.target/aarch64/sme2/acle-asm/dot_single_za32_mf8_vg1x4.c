/* { dg-do assemble { target { aarch64_asm_sme-f8f32_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f32_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sve2,+sme-f8f32"

/* Available variants are:
   _za32 if __ARM_FEATURE_SME_F8F32 != 0
   void svdot[_single]_za32[_mf8]_vg1x4_fpm (uint32_t slice, svmfloat8x4_t zn, svmfloat8_t zm, fpm_t fpm) __arm_streaming __arm_inout("za");  */
/*
** dot_single_0_z1_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fdot	za\.s\[\1, 0, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_0_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (0, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (0, z1, z0, fpm0))

/*
** dot_single_w0_z1_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fdot	za\.s\[\1, 0, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w0, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w0, z1, z0, fpm0))

/*
** dot_single_w8_z1_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 0, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w8, z1, z0, fpm0))

/*
** dot_single_w8p1_z1_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p1_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8 + 1, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w8 + 1, z1, z0, fpm0))

/*
** dot_single_w8p2_z20_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 2, vgx4\], {z20\.b - z23\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p2_z20_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8 + 2, z20, z0, fpm0),
		svdot_za32_vg1x4_fpm (w8 + 2, z20, z0, fpm0))

/*
** dot_single_w11p4_z27_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w11, 4, vgx4\], {z27\.b - z30\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w11p4_z27_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w11 + 4, z27, z0, fpm0),
		svdot_za32_vg1x4_fpm (w11 + 4, z27, z0, fpm0))

/*
** dot_single_w8p7_z1_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 7, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p7_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8 + 7, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w8 + 7, z1, z0, fpm0))

/*
** dot_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fdot	za\.s\[\1, 0, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p8_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8 + 8, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w8 + 8, z1, z0, fpm0))

/*
** dot_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	msr	fpmr, x1
**	fdot	za\.s\[\1, 0, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0m1_z1_z0, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w0 - 1, z1, z0, fpm0),
		svdot_za32_vg1x4_fpm (w0 - 1, z1, z0, fpm0))

/*
** dot_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 0, vgx4\], {z0\.b - z3\.b}, z15\.b
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (dot_single_w8_z0_z15, svmfloat8x4_t, svmfloat8_t,
		    svdot_single_za32_mf8_vg1x4_fpm (w8, z0, z15, fpm0),
		    svdot_za32_vg1x4_fpm (w8, z0, z15, fpm0))

/*
** dot_single_w8_z20_z16:
**	msr	fpmr, x1
**	mov	(z[0-7]).d, z16.d
**	fdot	za\.s\[w8, 0, vgx4\], {z20\.b - z23\.b}, \1\.b
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z20_z16, svmfloat8x4_t, svmfloat8_t,
		svdot_single_za32_mf8_vg1x4_fpm (w8, z20, z16, fpm0),
		svdot_za32_vg1x4_fpm (w8, z20, z16, fpm0))
