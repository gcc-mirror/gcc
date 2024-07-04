/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-f64f64"

#include "test_sme2_acle.h"

/*
** mla_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	fmla	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (mla_0_z0_z0, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (0, z0, z0),
	    svmla_za64_vg1x2 (0, z0, z0))

/*
** mla_w0_z0_z0:
**	mov	(w8|w9|w10|w11), w0
**	fmla	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (mla_w0_z0_z0, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w0, z0, z0),
	    svmla_za64_vg1x2 (w0, z0, z0))

/*
** mla_w8_z0_z4:
**	fmla	za\.d\[w8, 0, vgx2\], {z0\.d - z1\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_ZA_XN (mla_w8_z0_z4, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8, z0, z4),
	    svmla_za64_vg1x2 (w8, z0, z4))

/*
** mla_w8_z4_z18:
**	fmla	za\.d\[w8, 0, vgx2\], {z4\.d - z5\.d}, {z18\.d - z19\.d}
**	ret
*/
TEST_ZA_XN (mla_w8_z4_z18, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8, z4, z18),
	    svmla_za64_vg1x2 (w8, z4, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** mla_w8_z23_z0:
**	...
**	fmla	za\.d\[w8, 0, vgx2\], [^\n]+, {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (mla_w8_z23_z0, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8, z23, z0),
	    svmla_za64_vg1x2 (w8, z23, z0))

/*
** mla_w8_z18_z23:
**	...
**	fmla	za\.d\[w8, 0, vgx2\], {z18\.d - z19\.d}, [^\n]+
**	ret
*/
TEST_ZA_XN (mla_w8_z18_z23, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8, z18, z23),
	    svmla_za64_vg1x2 (w8, z18, z23))

/*
** mla_w8_z4_z28:
**	fmla	za\.d\[w8, 0, vgx2\], {z4\.d - z5\.d}, {z28\.d - z29\.d}
**	ret
*/
TEST_ZA_XN (mla_w8_z4_z28, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8, z4, z28),
	    svmla_za64_vg1x2 (w8, z4, z28))

/*
** mla_w8p7_z4_z0:
**	fmla	za\.d\[w8, 7, vgx2\], {z4\.d - z5\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (mla_w8p7_z4_z0, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8 + 7, z4, z0),
	    svmla_za64_vg1x2 (w8 + 7, z4, z0))

/*
** mla_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	fmla	za\.d\[\1, 0, vgx2\], {z4\.d - z5\.d}, {z4\.d - z5\.d}
**	ret
*/
TEST_ZA_XN (mla_w8p8_z4_z4, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8 + 8, z4, z4),
	    svmla_za64_vg1x2 (w8 + 8, z4, z4))

/*
** mla_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	fmla	za\.d\[\1, 0, vgx2\], {z4\.d - z5\.d}, {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (mla_w8m1_z4_z0, svfloat64x2_t,
	    svmla_za64_f64_vg1x2 (w8 - 1, z4, z0),
	    svmla_za64_vg1x2 (w8 - 1, z4, z0))

/*
** mla_single_0_z1_z0:
**	mov	(w8|w9|w10|w11), #?0
**	fmla	za\.d\[\1, 0, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_0_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (0, z1, z0),
	        svmla_za64_vg1x2 (0, z1, z0))

/*
** mla_single_w0_z1_z0:
**	mov	(w8|w9|w10|w11), w0
**	fmla	za\.d\[\1, 0, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w0, z1, z0),
	        svmla_za64_vg1x2 (w0, z1, z0))

/*
** mla_single_w8_z1_z0:
**	fmla	za\.d\[w8, 0, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w8, z1, z0),
	        svmla_za64_vg1x2 (w8, z1, z0))

/*
** mla_single_w8p7_z1_z0:
**	fmla	za\.d\[w8, 7, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p7_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w8 + 7, z1, z0),
	        svmla_za64_vg1x2 (w8 + 7, z1, z0))

/*
** mla_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	fmla	za\.d\[\1, 0, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p8_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w8 + 8, z1, z0),
	        svmla_za64_vg1x2 (w8 + 8, z1, z0))

/*
** mla_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	fmla	za\.d\[\1, 0, vgx2\], {z1\.d - z2\.d}, z0\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0m1_z1_z0, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w0 - 1, z1, z0),
	        svmla_za64_vg1x2 (w0 - 1, z1, z0))

/*
** mla_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	fmla	za\.d\[w8, 0, vgx2\], {z0\.d - z1\.d}, z15\.d
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (mla_single_w8_z0_z15, svfloat64x2_t, svfloat64_t,
		    svmla_single_za64_f64_vg1x2 (w8, z0, z15),
		    svmla_za64_vg1x2 (w8, z0, z15))

/*
** mla_single_w8_z20_z16:
**	mov	(z[0-7]).d, z16.d
**	fmla	za\.d\[w8, 0, vgx2\], {z20\.d - z21\.d}, \1\.d
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z20_z16, svfloat64x2_t, svfloat64_t,
	        svmla_single_za64_f64_vg1x2 (w8, z20, z16),
	        svmla_za64_vg1x2 (w8, z20, z16))
