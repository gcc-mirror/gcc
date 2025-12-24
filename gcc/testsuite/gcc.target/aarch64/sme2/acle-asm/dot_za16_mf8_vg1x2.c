/* { dg-do assemble { target { aarch64_asm_sme-f8f16_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f16_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sve2,+sme-f8f16"

/* Available variants are
   _za16 if __ARM_FEATURE_SME_F8F16 != 0
   void svdot_za16[_mf8]_vg1x2_fpm (uint32_t slice, svmfloat8x2_t zn, svmfloat8x2_t zm, fpm_t fpm) __arm_streaming __arm_inout("za");  */

/*
** dot_0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_0_z0_z0, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (0, z0, z0, fpm0),
	    svdot_za16_vg1x2_fpm (0, z0, z0, fpm0))

/*
** dot_w0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fdot	za\.h\[\1, 0, vgx2\], {z0\.b - z1\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w0_z0_z0, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w0, z0, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w0, z0, z0, fpm0))

/*
** dot_w8_z0_z4:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z0\.b - z1\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z4, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z0, z4, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z0, z4, fpm0))

/*
** dot_w8_z4_z18:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z4\.b - z5\.b}, {z18\.b - z19\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z4_z18, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z4, z18, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z4, z18, fpm0))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** dot_w8_z0_z23:
**	msr	fpmr, x1
**	...
**	fdot	za\.h\[w8, 0, vgx2\], {z0\.b - z1\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z23, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z0, z23, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z0, z23, fpm0))

/*
** dot_w8_z23_z0:
**	msr	fpmr, x1
**	...
**	fdot	za\.h\[w8, 0, vgx2\], [^\n]+, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z23_z0, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z23, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z23, z0, fpm0))

/*
** dot_w8_z18_z28:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z18\.b - z19\.b}, {z28\.b - z29\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z18_z28, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z18, z28, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z18, z28, fpm0))

/*
** dot_w8_z28_z4:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 0, vgx2\], {z28\.b - z29\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (dot_w8_z28_z4, svmfloat8x2_t, svdot_za16_mf8_vg1x2_fpm (w8, z28, z4, fpm0),
	    svdot_za16_vg1x2_fpm (w8, z28, z4, fpm0))

/*
** dot_w8p1_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 1, vgx2\], {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p1_z4_z0, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w8 + 1, z4, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w8 + 1, z4, z0, fpm0))

/*
** dot_w8p2_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 2, vgx2\], {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p2_z4_z0, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w8 + 2, z4, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w8 + 2, z4, z0, fpm0))

/*
** dot_w11p4_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.h\[w11, 4, vgx2\], {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w11p4_z4_z0, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w11 + 4, z4, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w11 + 4, z4, z0, fpm0))

/*
** dot_w8p7_z4_z0:
**	msr	fpmr, x1
**	fdot	za\.h\[w8, 7, vgx2\], {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p7_z4_z0, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w8 + 7, z4, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w8 + 7, z4, z0, fpm0))

/*
** dot_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fdot	za\.h\[\1, 0, vgx2\], {z4\.b - z5\.b}, {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (dot_w8p8_z4_z4, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w8 + 8, z4, z4, fpm0),
	    svdot_za16_vg1x2_fpm (w8 + 8, z4, z4, fpm0))

/*
** dot_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fdot	za\.h\[\1, 0, vgx2\], {z4\.b - z5\.b}, {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (dot_w8m1_z4_z0, svmfloat8x2_t,
	    svdot_za16_mf8_vg1x2_fpm (w8 - 1, z4, z0, fpm0),
	    svdot_za16_vg1x2_fpm (w8 - 1, z4, z0, fpm0))
