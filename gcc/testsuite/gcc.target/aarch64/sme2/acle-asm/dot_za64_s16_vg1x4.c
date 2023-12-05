/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** dot_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	sdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_0_z0_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (0, z0, z0),
	    svdot_za64_vg1x4 (0, z0, z0))

/*
** dot_w0_z0_z0:
**	mov	(w8|w9|w10|w11), w0
**	sdot	za\.d\[\1, 0, vgx4\], {z0\.h - z3\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w0_z0_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w0, z0, z0),
	    svdot_za64_vg1x4 (w0, z0, z0))

/*
** dot_w8_z0_z4:
**	sdot	za\.d\[w8, 0, vgx4\], {z0\.h - z3\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z4, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z0, z4),
	    svdot_za64_vg1x4 (w8, z0, z4))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** dot_w8_z0_z18:
**	...
**	sdot	za\.d\[w8, 0, vgx4\], {z0\.h - z3\.h}, [^\n]+
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z18, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z0, z18),
	    svdot_za64_vg1x4 (w8, z0, z18))

/*
** dot_w8_z18_z0:
**	...
**	sdot	za\.d\[w8, 0, vgx4\], [^\n]+, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z18_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z18, z0),
	    svdot_za64_vg1x4 (w8, z18, z0))

/*
** dot_w8_z0_z23:
**	...
**	sdot	za\.d\[w8, 0, vgx4\], {z0\.h - z3\.h}, [^\n]+
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z23, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z0, z23),
	    svdot_za64_vg1x4 (w8, z0, z23))

/*
** dot_w8_z23_z0:
**	...
**	sdot	za\.d\[w8, 0, vgx4\], [^\n]+, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z23_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z23, z0),
	    svdot_za64_vg1x4 (w8, z23, z0))

/*
** dot_w8_z4_z28:
**	sdot	za\.d\[w8, 0, vgx4\], {z4\.h - z7\.h}, {z28\.h - z31\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z4_z28, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z4, z28),
	    svdot_za64_vg1x4 (w8, z4, z28))

/*
** dot_w8_z28_z0:
**	sdot	za\.d\[w8, 0, vgx4\], {z28\.h - z31\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z28_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8, z28, z0),
	    svdot_za64_vg1x4 (w8, z28, z0))

/*
** dot_w8p1_z4_z0:
**	sdot	za\.d\[w8, 1, vgx4\], {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p1_z4_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8 + 1, z4, z0),
	    svdot_za64_vg1x4 (w8 + 1, z4, z0))

/*
** dot_w8p2_z4_z0:
**	sdot	za\.d\[w8, 2, vgx4\], {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p2_z4_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8 + 2, z4, z0),
	    svdot_za64_vg1x4 (w8 + 2, z4, z0))

/*
** dot_w11p4_z4_z0:
**	sdot	za\.d\[w11, 4, vgx4\], {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w11p4_z4_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w11 + 4, z4, z0),
	    svdot_za64_vg1x4 (w11 + 4, z4, z0))

/*
** dot_w8p7_z4_z0:
**	sdot	za\.d\[w8, 7, vgx4\], {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p7_z4_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8 + 7, z4, z0),
	    svdot_za64_vg1x4 (w8 + 7, z4, z0))

/*
** dot_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	sdot	za\.d\[\1, 0, vgx4\], {z4\.h - z7\.h}, {z4\.h - z7\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p8_z4_z4, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8 + 8, z4, z4),
	    svdot_za64_vg1x4 (w8 + 8, z4, z4))

/*
** dot_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	sdot	za\.d\[\1, 0, vgx4\], {z4\.h - z7\.h}, {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (dot_w8m1_z4_z0, svint16x4_t,
	    svdot_za64_s16_vg1x4 (w8 - 1, z4, z0),
	    svdot_za64_vg1x4 (w8 - 1, z4, z0))

/*
** dot_single_0_z1_z0:
**	mov	(w8|w9|w10|w11), #?0
**	sdot	za\.d\[\1, 0, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_0_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (0, z1, z0),
	        svdot_za64_vg1x4 (0, z1, z0))

/*
** dot_single_w0_z1_z0:
**	mov	(w8|w9|w10|w11), w0
**	sdot	za\.d\[\1, 0, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w0, z1, z0),
	        svdot_za64_vg1x4 (w0, z1, z0))

/*
** dot_single_w8_z1_z0:
**	sdot	za\.d\[w8, 0, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8, z1, z0),
	        svdot_za64_vg1x4 (w8, z1, z0))

/*
** dot_single_w8p1_z1_z0:
**	sdot	za\.d\[w8, 1, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p1_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8 + 1, z1, z0),
	        svdot_za64_vg1x4 (w8 + 1, z1, z0))

/*
** dot_single_w8p4_z20_z0:
**	sdot	za\.d\[w8, 4, vgx4\], {z20\.h - z23\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p4_z20_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8 + 4, z20, z0),
	        svdot_za64_vg1x4 (w8 + 4, z20, z0))

/*
** dot_single_w8p6_z27_z0:
**	sdot	za\.d\[w8, 6, vgx4\], {z27\.h - z30\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p6_z27_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8 + 6, z27, z0),
	        svdot_za64_vg1x4 (w8 + 6, z27, z0))

/*
** dot_single_w8p7_z1_z0:
**	sdot	za\.d\[w8, 7, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p7_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8 + 7, z1, z0),
	        svdot_za64_vg1x4 (w8 + 7, z1, z0))

/*
** dot_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	sdot	za\.d\[\1, 0, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p8_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8 + 8, z1, z0),
	        svdot_za64_vg1x4 (w8 + 8, z1, z0))

/*
** dot_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	sdot	za\.d\[\1, 0, vgx4\], {z1\.h - z4\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0m1_z1_z0, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w0 - 1, z1, z0),
	        svdot_za64_vg1x4 (w0 - 1, z1, z0))

/*
** dot_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	sdot	za\.d\[w8, 0, vgx4\], {z0\.h - z3\.h}, z15\.h
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (dot_single_w8_z0_z15, svint16x4_t, svint16_t,
		    svdot_single_za64_s16_vg1x4 (w8, z0, z15),
		    svdot_za64_vg1x4 (w8, z0, z15))

/*
** dot_single_w8_z20_z16:
**	mov	(z[0-7]).d, z16.d
**	sdot	za\.d\[w8, 0, vgx4\], {z20\.h - z23\.h}, \1\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z20_z16, svint16x4_t, svint16_t,
	        svdot_single_za64_s16_vg1x4 (w8, z20, z16),
	        svdot_za64_vg1x4 (w8, z20, z16))
