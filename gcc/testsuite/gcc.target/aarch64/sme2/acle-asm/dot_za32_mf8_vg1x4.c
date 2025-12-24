/* { dg-do assemble { target { aarch64_asm_sme-f8f32_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f32_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sve2,+sme-f8f32"

/* Available variants are
   _za32 if __ARM_FEATURE_SME_F8F32 != 0
   void svdot_za32[_mf8]_vg1x4_fpm (uint32_t slice, svmfloat8x4_t zn, svmfloat8x4_t zm, fpm_t fpm) __arm_streaming __arm_inout("za");  */

/*
** dot_0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_0_z0_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (0, z0, z0, fpm0),
	    svdot_za32_vg1x4_fpm (0, z0, z0, fpm0))

/*
** dot_w0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fdot	za\.s\[\1, 0, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w0_z0_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w0, z0, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w0, z0, z0, fpm0))

/*
** dot_w8_z0_z4:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 0, vgx4\], {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z4, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z0, z4, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z0, z4, fpm0))

/*
** dot_w8_z4_z18:
**	msr	fpmr, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fdot	za\.s\[w8, 0, vgx4\], {z4\.b - z7\.b}, {z28\.b - z31\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z4_z18, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z4, z18, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z4, z18, fpm0))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** dot_w8_z0_z23:
**	msr	fpmr, x1
**	...
**	fdot	za\.s\[w8, 0, vgx4\], {z0\.b - z3\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z23, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z0, z23, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z0, z23, fpm0))

/*
** dot_w8_z23_z0:
**	msr	fpmr, x1
**	...
**	fdot	za\.s\[w8, 0, vgx4\], [^\n]+, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z23_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z23, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z23, z0, fpm0))

/*
** dot_w8_z18_z28:
**	msr	fpmr, x1
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	fdot	za\.s\[w8, 0, vgx4\], {z24\.b - z27\.b}, {z28\.b - z31\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z18_z28, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z18, z28, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z18, z28, fpm0))

/*
** dot_w8_z28_z4:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 0, vgx4\], {z28\.b - z31\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z28_z4, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8, z28, z4, fpm0),
	    svdot_za32_vg1x4_fpm (w8, z28, z4, fpm0))

/*
** dot_w8p1_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 1, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p1_z4_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8 + 1, z4, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w8 + 1, z4, z0, fpm0))

/*
** dot_w8p2_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 2, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p2_z4_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8 + 2, z4, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w8 + 2, z4, z0, fpm0))

/*
** dot_w11p4_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w11, 4, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w11p4_z4_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w11 + 4, z4, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w11 + 4, z4, z0, fpm0))

/*
** dot_w8p7_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.s\[w8, 7, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p7_z4_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8 + 7, z4, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w8 + 7, z4, z0, fpm0))

/*
** dot_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fdot	za\.s\[\1, 0, vgx4\], {z4\.b - z7\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p8_z4_z4, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8 + 8, z4, z4, fpm0),
	    svdot_za32_vg1x4_fpm (w8 + 8, z4, z4, fpm0))

/*
** dot_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fdot	za\.s\[\1, 0, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (dot_w8m1_z4_z0, svmfloat8x4_t,
	    svdot_za32_mf8_vg1x4_fpm (w8 - 1, z4, z0, fpm0),
	    svdot_za32_vg1x4_fpm (w8 - 1, z4, z0, fpm0))
