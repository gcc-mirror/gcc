/* { dg-do assemble { target { aarch64_asm_sme-f8f32_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f32_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme+sme-f8f32"

/*
** mla_0_z0_z0:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fmlall	za\.s\[\1, 0:3\], z0\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_0_z0_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (0, z0, z0, fpm0),
	    svmla_za32_vg4x1_fpm (0, z0, z0, fpm0))

/*
** mla_w0_z0_z3:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fmlall	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mla_w0_z0_z3, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w0, z0, z3, fpm0),
	    svmla_za32_vg4x1_fpm (w0, z0, z3, fpm0))

/*
** mla_w7_z0_z3:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w7
**	fmlall	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mla_w7_z0_z3, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w7, z0, z3, fpm0),
	    svmla_za32_vg4x1_fpm (w7, z0, z3, fpm0))

/*
** mla_w8_z7_z3:
**	msr	fpmr, x1
**	fmlall	za\.s\[w8, 0:3\], z7\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mla_w8_z7_z3, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8, z7, z3, fpm0),
	    svmla_za32_vg4x1_fpm (w8, z7, z3, fpm0))

/*
** mla_w8_z31_z16:
**	msr	fpmr, x1
**	mov	(z[0-7])\.d, z16\.d
**	fmlall	za\.s\[w8, 0:3\], z31\.b. \1\.b
**	ret
*/
TEST_ZA_X1 (mla_w8_z31_z16, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8, z31, z16, fpm0),
	    svmla_za32_vg4x1_fpm (w8, z31, z16, fpm0))

/*
** mla_w8p1_z0_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z0\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w8p1_z0_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8 + 1, z0, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w8 + 1, z0, z0, fpm0))

/*
** mla_w10p4_z23_z0:
**	msr	fpmr, x1
**	fmlall	za\.s\[w10, 4:7\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w10p4_z23_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w10 + 4, z23, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w10 + 4, z23, z0, fpm0))

/*
** mla_w11p6_z23_z0:
**	add	(w8|w9|w10|w11), w11, #?6
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w11p6_z23_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w11 + 6, z23, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w11 + 6, z23, z0, fpm0))

/*
** mla_w9p8_z7_z7:
**	msr	fpmr, x1
**	fmlall	za\.s\[w9, 8:11\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mla_w9p8_z7_z7, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w9 + 8, z7, z7, fpm0),
	    svmla_za32_vg4x1_fpm (w9 + 8, z7, z7, fpm0))

/*
** mla_w11p12_z23_z0:
**	msr	fpmr, x1
**	fmlall	za\.s\[w11, 12:15\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w11p12_z23_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w11 + 12, z23, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w11 + 12, z23, z0, fpm0))

/*
** mla_w8p14_z23_z0:
**	add	(w8|w9|w10|w11), w8, #?14
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w8p14_z23_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8 + 14, z23, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w8 + 14, z23, z0, fpm0))

/*
** mla_w8p15_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?15
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mla_w8p15_z7_z7, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8 + 15, z7, z7, fpm0),
	    svmla_za32_vg4x1_fpm (w8 + 15, z7, z7, fpm0))

/*
** mla_w8p16_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?16
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mla_w8p16_z7_z7, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8 + 16, z7, z7, fpm0),
	    svmla_za32_vg4x1_fpm (w8 + 16, z7, z7, fpm0))

/*
** mla_w8m1_z16_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	msr	fpmr, x1
**	fmlall	za\.s\[\1, 0:3\], z16\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mla_w8m1_z16_z0, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w8 - 1, z16, z0, fpm0),
	    svmla_za32_vg4x1_fpm (w8 - 1, z16, z0, fpm0))

/*
** mla_w12_z0_z3:
**	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w12
**	fmlall	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mla_w12_z0_z3, svmfloat8_t,
	    svmla_za32_mf8_vg4x1_fpm (w12, z0, z3, fpm0),
	    svmla_za32_vg4x1_fpm (w12, z0, z3, fpm0))
