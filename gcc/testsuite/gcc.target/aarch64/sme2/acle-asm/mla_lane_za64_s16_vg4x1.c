/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** mla_lane_0_z0_z0_0:
**	mov	(w8|w9|w10|w11), #?0
**	smlall	za\.d\[\1, 0:3\], z0\.h, z0\.h\[0\]
**	ret
*/
TEST_ZA_X1 (mla_lane_0_z0_z0_0, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (0, z0, z0, 0),
	    svmla_lane_za64_vg4x1 (0, z0, z0, 0))

/*
** mla_lane_w0_z0_z3_1:
**	mov	(w8|w9|w10|w11), w0
**	smlall	za\.d\[\1, 0:3\], z0\.h, z3\.h\[1\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w0_z0_z3_1, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w0, z0, z3, 1),
	    svmla_lane_za64_vg4x1 (w0, z0, z3, 1))

/*
** mla_lane_w7_z0_z3_2:
**	mov	(w8|w9|w10|w11), w7
**	smlall	za\.d\[\1, 0:3\], z0\.h, z3\.h\[2\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w7_z0_z3_2, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w7, z0, z3, 2),
	    svmla_lane_za64_vg4x1 (w7, z0, z3, 2))

/*
** mla_lane_w8_z7_z3_3:
**	smlall	za\.d\[w8, 0:3\], z7\.h, z3\.h\[3\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8_z7_z3_3, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8, z7, z3, 3),
	    svmla_lane_za64_vg4x1 (w8, z7, z3, 3))

/*
** mla_lane_w8_z31_z16_4:
**	mov	(z[0-7])\.d, z16\.d
**	smlall	za\.d\[w8, 0:3\], z31\.h. \1\.h\[4\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8_z31_z16_4, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8, z31, z16, 4),
	    svmla_lane_za64_vg4x1 (w8, z31, z16, 4))

/*
** mla_lane_w8p1_z0_z0_5:
**	add	(w8|w9|w10|w11), w8, #?1
**	smlall	za\.d\[\1, 0:3\], z0\.h, z0\.h\[5\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p1_z0_z0_5, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 1, z0, z0, 5),
	    svmla_lane_za64_vg4x1 (w8 + 1, z0, z0, 5))

/*
** mla_lane_w8p2_z23_z0_6:
**	add	(w8|w9|w10|w11), w8, #?2
**	smlall	za\.d\[\1, 0:3\], z23\.h, z0\.h\[6\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p2_z23_z0_6, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 2, z23, z0, 6),
	    svmla_lane_za64_vg4x1 (w8 + 2, z23, z0, 6))

/*
** mla_lane_w11p4_z23_z0_7:
**	smlall	za\.d\[w11, 4:7\], z23\.h, z0\.h\[7\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w11p4_z23_z0_7, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w11 + 4, z23, z0, 7),
	    svmla_lane_za64_vg4x1 (w11 + 4, z23, z0, 7))

/*
** mla_lane_w8p7_z7_z7_0:
**	add	(w8|w9|w10|w11), w8, #?7
**	smlall	za\.d\[\1, 0:3\], z7\.h, z7\.h\[0\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p7_z7_z7_0, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 7, z7, z7, 0),
	    svmla_lane_za64_vg4x1 (w8 + 7, z7, z7, 0))

/*
** mla_lane_w11p12_z23_z0_1:
**	smlall	za\.d\[w11, 12:15\], z23\.h, z0\.h\[1\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w11p12_z23_z0_1, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w11 + 12, z23, z0, 1),
	    svmla_lane_za64_vg4x1 (w11 + 12, z23, z0, 1))

/*
** mla_lane_w8p14_z23_z0_2:
**	add	(w8|w9|w10|w11), w8, #?14
**	smlall	za\.d\[w8, 0:3\], z23\.h, z0\.h\[2\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p14_z23_z0_2, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 14, z23, z0, 2),
	    svmla_lane_za64_vg4x1 (w8 + 14, z23, z0, 2))

/*
** mla_lane_w8p15_z7_z7_3:
**	add	(w8|w9|w10|w11), w8, #?15
**	smlall	za\.d\[\1, 0:3\], z7\.h, z7\.h\[3\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p15_z7_z7_3, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 15, z7, z7, 3),
	    svmla_lane_za64_vg4x1 (w8 + 15, z7, z7, 3))

/*
** mla_lane_w8p16_z7_z7_4:
**	add	(w8|w9|w10|w11), w8, #?16
**	smlall	za\.d\[\1, 0:3\], z7\.h, z7\.h\[4\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8p16_z7_z7_4, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 + 16, z7, z7, 4),
	    svmla_lane_za64_vg4x1 (w8 + 16, z7, z7, 4))

/*
** mla_lane_w8m1_z16_z0_5:
**	sub	(w8|w9|w10|w11), w8, #?1
**	smlall	za\.d\[\1, 0:3\], z16\.h, z0\.h\[5\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w8m1_z16_z0_5, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w8 - 1, z16, z0, 5),
	    svmla_lane_za64_vg4x1 (w8 - 1, z16, z0, 5))

/*
** mla_lane_w12_z0_z3_6:
**	mov	(w8|w9|w10|w11), w12
**	smlall	za\.d\[\1, 0:3\], z0\.h, z3\.h\[6\]
**	ret
*/
TEST_ZA_X1 (mla_lane_w12_z0_z3_6, svint16_t,
	    svmla_lane_za64_s16_vg4x1 (w12, z0, z3, 6),
	    svmla_lane_za64_vg4x1 (w12, z0, z3, 6))
