/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** dot_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfdot	za\.s\[\1, 0, vgx2\], {z0\.h - z1\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_0_z0_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (0, z0, z0),
	    svdot_za32_vg1x2 (0, z0, z0))

/*
** dot_w0_z0_z0:
**	mov	(w8|w9|w10|w11), w0
**	bfdot	za\.s\[\1, 0, vgx2\], {z0\.h - z1\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w0_z0_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w0, z0, z0),
	    svdot_za32_vg1x2 (w0, z0, z0))

/*
** dot_w8_z0_z4:
**	bfdot	za\.s\[w8, 0, vgx2\], {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z4, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z0, z4),
	    svdot_za32_vg1x2 (w8, z0, z4))

/*
** dot_w8_z4_z18:
**	bfdot	za\.s\[w8, 0, vgx2\], {z4\.h - z5\.h}, {z18\.h - z19\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z4_z18, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z4, z18),
	    svdot_za32_vg1x2 (w8, z4, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** dot_w8_z0_z23:
**	...
**	bfdot	za\.s\[w8, 0, vgx2\], {z0\.h - z1\.h}, [^\n]+
**	ret
*/
TEST_ZA_XN (dot_w8_z0_z23, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z0, z23),
	    svdot_za32_vg1x2 (w8, z0, z23))

/*
** dot_w8_z23_z0:
**	...
**	bfdot	za\.s\[w8, 0, vgx2\], [^\n]+, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z23_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z23, z0),
	    svdot_za32_vg1x2 (w8, z23, z0))

/*
** dot_w8_z18_z28:
**	bfdot	za\.s\[w8, 0, vgx2\], {z18\.h - z19\.h}, {z28\.h - z29\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z18_z28, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z18, z28),
	    svdot_za32_vg1x2 (w8, z18, z28))

/*
** dot_w8_z28_z4:
**	bfdot	za\.s\[w8, 0, vgx2\], {z28\.h - z29\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_ZA_XN (dot_w8_z28_z4, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8, z28, z4),
	    svdot_za32_vg1x2 (w8, z28, z4))

/*
** dot_w8p1_z4_z0:
**	bfdot	za\.s\[w8, 1, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p1_z4_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8 + 1, z4, z0),
	    svdot_za32_vg1x2 (w8 + 1, z4, z0))

/*
** dot_w8p2_z4_z0:
**	bfdot	za\.s\[w8, 2, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p2_z4_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8 + 2, z4, z0),
	    svdot_za32_vg1x2 (w8 + 2, z4, z0))

/*
** dot_w11p4_z4_z0:
**	bfdot	za\.s\[w11, 4, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w11p4_z4_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w11 + 4, z4, z0),
	    svdot_za32_vg1x2 (w11 + 4, z4, z0))

/*
** dot_w8p7_z4_z0:
**	bfdot	za\.s\[w8, 7, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p7_z4_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8 + 7, z4, z0),
	    svdot_za32_vg1x2 (w8 + 7, z4, z0))

/*
** dot_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfdot	za\.s\[\1, 0, vgx2\], {z4\.h - z5\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_ZA_XN (dot_w8p8_z4_z4, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8 + 8, z4, z4),
	    svdot_za32_vg1x2 (w8 + 8, z4, z4))

/*
** dot_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	bfdot	za\.s\[\1, 0, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (dot_w8m1_z4_z0, svbfloat16x2_t,
	    svdot_za32_bf16_vg1x2 (w8 - 1, z4, z0),
	    svdot_za32_vg1x2 (w8 - 1, z4, z0))

/*
** dot_single_0_z1_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfdot	za\.s\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_0_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (0, z1, z0),
	        svdot_za32_vg1x2 (0, z1, z0))

/*
** dot_single_w0_z1_z0:
**	mov	(w8|w9|w10|w11), w0
**	bfdot	za\.s\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w0, z1, z0),
	        svdot_za32_vg1x2 (w0, z1, z0))

/*
** dot_single_w8_z1_z0:
**	bfdot	za\.s\[w8, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8, z1, z0),
	        svdot_za32_vg1x2 (w8, z1, z0))

/*
** dot_single_w8p1_z1_z0:
**	bfdot	za\.s\[w8, 1, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p1_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8 + 1, z1, z0),
	        svdot_za32_vg1x2 (w8 + 1, z1, z0))

/*
** dot_single_w8p2_z20_z0:
**	bfdot	za\.s\[w8, 2, vgx2\], {z20\.h - z21\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p2_z20_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8 + 2, z20, z0),
	        svdot_za32_vg1x2 (w8 + 2, z20, z0))

/*
** dot_single_w11p4_z27_z0:
**	bfdot	za\.s\[w11, 4, vgx2\], {z27\.h - z28\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w11p4_z27_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w11 + 4, z27, z0),
	        svdot_za32_vg1x2 (w11 + 4, z27, z0))

/*
** dot_single_w8p7_z1_z0:
**	bfdot	za\.s\[w8, 7, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p7_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8 + 7, z1, z0),
	        svdot_za32_vg1x2 (w8 + 7, z1, z0))

/*
** dot_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfdot	za\.s\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8p8_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8 + 8, z1, z0),
	        svdot_za32_vg1x2 (w8 + 8, z1, z0))

/*
** dot_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	bfdot	za\.s\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w0m1_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w0 - 1, z1, z0),
	        svdot_za32_vg1x2 (w0 - 1, z1, z0))

/*
** dot_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	bfdot	za\.s\[w8, 0, vgx2\], {z0\.h - z1\.h}, z15\.h
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (dot_single_w8_z0_z15, svbfloat16x2_t, svbfloat16_t,
		    svdot_single_za32_bf16_vg1x2 (w8, z0, z15),
		    svdot_za32_vg1x2 (w8, z0, z15))

/*
** dot_single_w8_z20_z16:
**	mov	(z[0-7]).d, z16.d
**	bfdot	za\.s\[w8, 0, vgx2\], {z20\.h - z21\.h}, \1\.h
**	ret
*/
TEST_ZA_SINGLE (dot_single_w8_z20_z16, svbfloat16x2_t, svbfloat16_t,
	        svdot_single_za32_bf16_vg1x2 (w8, z20, z16),
	        svdot_za32_vg1x2 (w8, z20, z16))
