/* { dg-do assemble { target aarch64_asm_sme-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-b16b16"

/*
** mla_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (mla_0_z0_z0, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (0, z0, z0),
	    svmla_za16_vg1x2 (0, z0, z0))

/*
** mla_w0_z0_z0:
**	mov	(w8|w9|w10|w11), w0
**	bfmla	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (mla_w0_z0_z0, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w0, z0, z0),
	    svmla_za16_vg1x2 (w0, z0, z0))

/*
** mla_w8_z0_z4:
**	bfmla	za\.h\[w8, 0, vgx2\], {z0\.h - z1\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_ZA_XN (mla_w8_z0_z4, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8, z0, z4),
	    svmla_za16_vg1x2 (w8, z0, z4))

/*
** mla_w8_z4_z18:
**	bfmla	za\.h\[w8, 0, vgx2\], {z4\.h - z5\.h}, {z18\.h - z19\.h}
**	ret
*/
TEST_ZA_XN (mla_w8_z4_z18, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8, z4, z18),
	    svmla_za16_vg1x2 (w8, z4, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** mla_w8_z23_z0:
**	...
**	bfmla	za\.h\[w8, 0, vgx2\], [^\n]+, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (mla_w8_z23_z0, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8, z23, z0),
	    svmla_za16_vg1x2 (w8, z23, z0))

/*
** mla_w8_z18_z23:
**	...
**	bfmla	za\.h\[w8, 0, vgx2\], {z18\.h - z19\.h}, [^\n]+
**	ret
*/
TEST_ZA_XN (mla_w8_z18_z23, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8, z18, z23),
	    svmla_za16_vg1x2 (w8, z18, z23))

/*
** mla_w8_z4_z28:
**	bfmla	za\.h\[w8, 0, vgx2\], {z4\.h - z5\.h}, {z28\.h - z29\.h}
**	ret
*/
TEST_ZA_XN (mla_w8_z4_z28, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8, z4, z28),
	    svmla_za16_vg1x2 (w8, z4, z28))

/*
** mla_w8p7_z4_z0:
**	bfmla	za\.h\[w8, 7, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (mla_w8p7_z4_z0, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8 + 7, z4, z0),
	    svmla_za16_vg1x2 (w8 + 7, z4, z0))

/*
** mla_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfmla	za\.h\[\1, 0, vgx2\], {z4\.h - z5\.h}, {z4\.h - z5\.h}
**	ret
*/
TEST_ZA_XN (mla_w8p8_z4_z4, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8 + 8, z4, z4),
	    svmla_za16_vg1x2 (w8 + 8, z4, z4))

/*
** mla_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	bfmla	za\.h\[\1, 0, vgx2\], {z4\.h - z5\.h}, {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (mla_w8m1_z4_z0, svbfloat16x2_t,
	    svmla_za16_bf16_vg1x2 (w8 - 1, z4, z0),
	    svmla_za16_vg1x2 (w8 - 1, z4, z0))

/*
** mla_single_0_z1_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfmla	za\.h\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_0_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (0, z1, z0),
	        svmla_za16_vg1x2 (0, z1, z0))

/*
** mla_single_w0_z1_z0:
**	mov	(w8|w9|w10|w11), w0
**	bfmla	za\.h\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w0, z1, z0),
	        svmla_za16_vg1x2 (w0, z1, z0))

/*
** mla_single_w8_z1_z0:
**	bfmla	za\.h\[w8, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w8, z1, z0),
	        svmla_za16_vg1x2 (w8, z1, z0))

/*
** mla_single_w8p7_z1_z0:
**	bfmla	za\.h\[w8, 7, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p7_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w8 + 7, z1, z0),
	        svmla_za16_vg1x2 (w8 + 7, z1, z0))

/*
** mla_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfmla	za\.h\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p8_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w8 + 8, z1, z0),
	        svmla_za16_vg1x2 (w8 + 8, z1, z0))

/*
** mla_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	bfmla	za\.h\[\1, 0, vgx2\], {z1\.h - z2\.h}, z0\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0m1_z1_z0, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w0 - 1, z1, z0),
	        svmla_za16_vg1x2 (w0 - 1, z1, z0))

/*
** mla_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	bfmla	za\.h\[w8, 0, vgx2\], {z0\.h - z1\.h}, z15\.h
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (mla_single_w8_z0_z15, svbfloat16x2_t, svbfloat16_t,
		    svmla_single_za16_bf16_vg1x2 (w8, z0, z15),
		    svmla_za16_vg1x2 (w8, z0, z15))

/*
** mla_single_w8_z20_z16:
**	mov	(z[0-7]).d, z16.d
**	bfmla	za\.h\[w8, 0, vgx2\], {z20\.h - z21\.h}, \1\.h
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z20_z16, svbfloat16x2_t, svbfloat16_t,
	        svmla_single_za16_bf16_vg1x2 (w8, z20, z16),
	        svmla_za16_vg1x2 (w8, z20, z16))
