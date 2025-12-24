/* { dg-do assemble { target { aarch64_asm_sme-f8f16_ok } } } */
/* { dg-do compile { target { ! { aarch64_asm_sme-f8f16_ok } } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme+sme-f8f16"

/*
** mla_lane_0_z0_z0_0:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), #?0
**	fmlal	za\.h\[\1, 0:1\], z0\.b, z0\.b\[0\]
**	ret
*/

TEST_ZA_X1 (mla_lane_0_z0_z0_0, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (0, z0, z0, 0, fpm0),
	    svmla_lane_za16_vg2x1_fpm (0, z0, z0, 0, fpm0))

/*
** mla_lane_w0_z0_z3_1:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w0
**	fmlal	za\.h\[\1, 0:1\], z0\.b, z3\.b\[1\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w0_z0_z3_1, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w0, z0, z3, 1, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w0, z0, z3, 1, fpm0))

/*
** mla_lane_w7_z0_z3_2:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w7
**	fmlal	za\.h\[\1, 0:1\], z0\.b, z3\.b\[2\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w7_z0_z3_2, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w7, z0, z3, 2, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w7, z0, z3, 2, fpm0))

/*
** mla_lane_w8_z7_z3_3:
** 	msr	fpmr, x1
**	fmlal	za\.h\[w8, 0:1\], z7\.b, z3\.b\[3\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8_z7_z3_3, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8, z7, z3, 3, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8, z7, z3, 3, fpm0))

/*
** mla_lane_w8_z31_z16_4:
** 	msr	fpmr, x1
**	mov	(z[0-7])\.d, z16\.d
**	fmlal	za\.h\[w8, 0:1\], z31\.b. \1\.b\[4\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8_z31_z16_4, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8, z31, z16, 4, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8, z31, z16, 4, fpm0))

/*
** mla_lane_w8p1_z0_z0_5:
**	add	(w8|w9|w10|w11), w8, #?1
** 	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1\], z0\.b, z0\.b\[5\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p1_z0_z0_5, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 1, z0, z0, 5, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 1, z0, z0, 5, fpm0))

/*
** mla_lane_w8p2_z23_z0_6:
** 	msr	fpmr, x1
**	fmlal	za\.h\[w8, 2:3\], z23\.b, z0\.b\[6\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p2_z23_z0_6, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 2, z23, z0, 6, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 2, z23, z0, 6, fpm0))

/*
** mla_lane_w11p6_z23_z0_7:
** 	msr	fpmr, x1
**	fmlal	za\.h\[w11, 6:7\], z23\.b, z0\.b\[7\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w11p6_z23_z0_7, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w11 + 6, z23, z0, 7, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w11 + 6, z23, z0, 7, fpm0))

/*
** mla_lane_w8p7_z7_z7_8:
**	add	(w8|w9|w10|w11), w8, #?7
** 	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1\], z7\.b, z7\.b\[8\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p7_z7_z7_8, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 7, z7, z7, 8, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 7, z7, z7, 8, fpm0))

/*
** mla_lane_w11p12_z23_z0_7:
** 	msr	fpmr, x1
**	fmlal	za\.h\[w11, 12:13\], z23\.b, z0\.b\[7\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w11p12_z23_z0_7, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w11 + 12, z23, z0, 7, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w11 + 12, z23, z0, 7, fpm0))

/*
** mla_lane_w8p14_z23_z0_10:
** 	msr	fpmr, x1
**	fmlal	za\.h\[w8, 14:15\], z23\.b, z0\.b\[10\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p14_z23_z0_10, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 14, z23, z0, 10, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 14, z23, z0, 10, fpm0))

/*
** mla_lane_w8p15_z7_z7_11:
**	add	(w8|w9|w10|w11), w8, #?15
** 	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1\], z7\.b, z7\.b\[11\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p15_z7_z7_11, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 15, z7, z7, 11, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 15, z7, z7, 11, fpm0))

/*
** mla_lane_w8p16_z7_z7_12:
**	add	(w8|w9|w10|w11), w8, #?16
** 	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1\], z7\.b, z7\.b\[12\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p16_z7_z7_12, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 + 16, z7, z7, 12, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 + 16, z7, z7, 12, fpm0))

/*
** mla_lane_w8m1_z16_z0_13:
**	sub	(w8|w9|w10|w11), w8, #?1
** 	msr	fpmr, x1
**	fmlal	za\.h\[\1, 0:1\], z16\.b, z0\.b\[13\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8m1_z16_z0_13, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w8 - 1, z16, z0, 13, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w8 - 1, z16, z0, 13, fpm0))

/*
** mla_lane_w12_z0_z3_15:
** 	msr	fpmr, x1
**	mov	(w8|w9|w10|w11), w12
**	fmlal	za\.h\[\1, 0:1\], z0\.b, z3\.b\[15\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w12_z0_z3_15, svmfloat8_t,
	    svmla_lane_za16_mf8_vg2x1_fpm (w12, z0, z3, 15, fpm0),
	    svmla_lane_za16_vg2x1_fpm (w12, z0, z3, 15, fpm0))
