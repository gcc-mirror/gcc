/* { dg-do assemble { target { aarch64_asm_sme-f8f16_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f16_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme+sme-f8f16"

/*
** mla_0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_0_z0_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (0, z0, z0, fpm0),
	    svmla_za16_vg2x4_fpm (0, z0, z0, fpm0))

/*
** mla_w0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w0_z0_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w0, z0, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w0, z0, z0, fpm0))

/*
** mla_w8_z0_z4:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (mla_w8_z0_z4, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z0, z4, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z0, z4, fpm0))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** mla_w8_z0_z18:
**	msr	fpmr, x1
**	...
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z0\.b - z3\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (mla_w8_z0_z18, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z0, z18, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z0, z18, fpm0))

/*
** mla_w8_z18_z0:
**	msr	fpmr, x1
**	...
**	fmlal	za\.h\[w8, 0:1, vgx4\], [^\n]+, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8_z18_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z18, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z18, z0, fpm0))

/*
** mla_w8_z0_z23:
**	msr	fpmr, x1
**	...
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z0\.b - z3\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (mla_w8_z0_z23, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z0, z23, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z0, z23, fpm0))

/*
** mla_w8_z23_z0:
**	msr	fpmr, x1
**	...
**	fmlal	za\.h\[w8, 0:1, vgx4\], [^\n]+, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8_z23_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z23, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z23, z0, fpm0))

/*
** mla_w8_z4_z28:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z4\.b - z7\.b}, {z28\.b - z31\.b}
**	ret
*/
TEST_ZA_XN (mla_w8_z4_z28, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z4, z28, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z4, z28, fpm0))

/*
** mla_w8_z28_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z28\.b - z31\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8_z28_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8, z28, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8, z28, z0, fpm0))

/*
** mla_w8p1_z4_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8p1_z4_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8 + 1, z4, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8 + 1, z4, z0, fpm0))

/*
** mla_w8p2_z4_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 2:3, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8p2_z4_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8 + 2, z4, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8 + 2, z4, z0, fpm0))

/*
** mla_w11p6_z4_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w11, 6:7, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w11p6_z4_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w11 + 6, z4, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w11 + 6, z4, z0, fpm0))

/*
** mla_w8p7_z4_z0:
**	add	(w8|w9|w10|w11), w8, #?7
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8p7_z4_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8 + 7, z4, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8 + 7, z4, z0, fpm0))

/*
** mla_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z4\.b - z7\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (mla_w8p8_z4_z4, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8 + 8, z4, z4, fpm0),
	    svmla_za16_vg2x4_fpm (w8 + 8, z4, z4, fpm0))

/*
** mla_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mla_w8m1_z4_z0, svmfloat8x4_t,
	    svmla_za16_mf8_vg2x4_fpm (w8 - 1, z4, z0, fpm0),
	    svmla_za16_vg2x4_fpm (w8 - 1, z4, z0, fpm0))

/*
** mla_single_0_z1_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_0_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (0, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (0, z1, z0, fpm0))

/*
** mla_single_w0_z1_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w0, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w0, z1, z0, fpm0))

/*
** mla_single_w8_z1_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8, z1, z0, fpm0))

/*
** mla_single_w8p1_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p1_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8 + 1, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8 + 1, z1, z0, fpm0))

/*
** mla_single_w8p2_z20_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 2:3, vgx4\], {z20\.b - z23\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p2_z20_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8 + 2, z20, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8 + 2, z20, z0, fpm0))

/*
** mla_single_w8p6_z27_z0:
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 6:7, vgx4\], {z27\.b - z30\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p6_z27_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8 + 6, z27, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8 + 6, z27, z0, fpm0))

/*
** mla_single_w8p7_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?7
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p7_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8 + 7, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8 + 7, z1, z0, fpm0))

/*
** mla_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8p8_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8 + 8, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w8 + 8, z1, z0, fpm0))

/*
** mla_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w0m1_z1_z0, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w0 - 1, z1, z0, fpm0),
	        svmla_za16_vg2x4_fpm (w0 - 1, z1, z0, fpm0))

/*
** mla_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z0\.b - z3\.b}, z15\.b
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (mla_single_w8_z0_z15, svmfloat8x4_t, svmfloat8_t,
		    svmla_single_za16_mf8_vg2x4_fpm (w8, z0, z15, fpm0),
		    svmla_za16_vg2x4_fpm (w8, z0, z15, fpm0))

/*
** mla_single_w8_z20_z16:
**	msr	fpmr, x1
**	mov	(z[0-7]).d, z16.d
**	fmlal	za\.h\[w8, 0:1, vgx4\], {z20\.b - z23\.b}, \1\.b
**	ret
*/
TEST_ZA_SINGLE (mla_single_w8_z20_z16, svmfloat8x4_t, svmfloat8_t,
	        svmla_single_za16_mf8_vg2x4_fpm (w8, z20, z16, fpm0),
	        svmla_za16_vg2x4_fpm (w8, z20, z16, fpm0))
