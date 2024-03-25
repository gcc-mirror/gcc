/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_lane_0_z0_z0_0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsl	za\.s\[\1, 0:1\], z0\.h, z0\.h\[0\]
**	ret
*/
TEST_ZA_X1 (mls_lane_0_z0_z0_0, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (0, z0, z0, 0),
	    svmls_lane_za32_vg2x1 (0, z0, z0, 0))

/*
** mls_lane_w0_z0_z3_1:
**	mov	(w8|w9|w10|w11), w0
**	umlsl	za\.s\[\1, 0:1\], z0\.h, z3\.h\[1\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w0_z0_z3_1, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w0, z0, z3, 1),
	    svmls_lane_za32_vg2x1 (w0, z0, z3, 1))

/*
** mls_lane_w7_z0_z3_2:
**	mov	(w8|w9|w10|w11), w7
**	umlsl	za\.s\[\1, 0:1\], z0\.h, z3\.h\[2\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w7_z0_z3_2, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w7, z0, z3, 2),
	    svmls_lane_za32_vg2x1 (w7, z0, z3, 2))

/*
** mls_lane_w8_z7_z3_3:
**	umlsl	za\.s\[w8, 0:1\], z7\.h, z3\.h\[3\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8_z7_z3_3, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8, z7, z3, 3),
	    svmls_lane_za32_vg2x1 (w8, z7, z3, 3))

/*
** mls_lane_w8_z31_z16_4:
**	mov	(z[0-7])\.d, z16\.d
**	umlsl	za\.s\[w8, 0:1\], z31\.h. \1\.h\[4\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8_z31_z16_4, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8, z31, z16, 4),
	    svmls_lane_za32_vg2x1 (w8, z31, z16, 4))

/*
** mls_lane_w8p1_z0_z0_5:
**	add	(w8|w9|w10|w11), w8, #?1
**	umlsl	za\.s\[\1, 0:1\], z0\.h, z0\.h\[5\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p1_z0_z0_5, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 1, z0, z0, 5),
	    svmls_lane_za32_vg2x1 (w8 + 1, z0, z0, 5))

/*
** mls_lane_w8p2_z23_z0_6:
**	umlsl	za\.s\[w8, 2:3\], z23\.h, z0\.h\[6\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p2_z23_z0_6, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 2, z23, z0, 6),
	    svmls_lane_za32_vg2x1 (w8 + 2, z23, z0, 6))

/*
** mls_lane_w11p6_z23_z0_7:
**	umlsl	za\.s\[w11, 6:7\], z23\.h, z0\.h\[7\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w11p6_z23_z0_7, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w11 + 6, z23, z0, 7),
	    svmls_lane_za32_vg2x1 (w11 + 6, z23, z0, 7))

/*
** mls_lane_w8p7_z7_z7_0:
**	add	(w8|w9|w10|w11), w8, #?7
**	umlsl	za\.s\[\1, 0:1\], z7\.h, z7\.h\[0\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p7_z7_z7_0, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 7, z7, z7, 0),
	    svmls_lane_za32_vg2x1 (w8 + 7, z7, z7, 0))

/*
** mls_lane_w11p10_z23_z0_1:
**	umlsl	za\.s\[w11, 10:11\], z23\.h, z0\.h\[1\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w11p10_z23_z0_1, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w11 + 10, z23, z0, 1),
	    svmls_lane_za32_vg2x1 (w11 + 10, z23, z0, 1))

/*
** mls_lane_w8p14_z23_z0_2:
**	umlsl	za\.s\[w8, 14:15\], z23\.h, z0\.h\[2\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p14_z23_z0_2, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 14, z23, z0, 2),
	    svmls_lane_za32_vg2x1 (w8 + 14, z23, z0, 2))

/*
** mls_lane_w8p15_z7_z7_3:
**	add	(w8|w9|w10|w11), w8, #?15
**	umlsl	za\.s\[\1, 0:1\], z7\.h, z7\.h\[3\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p15_z7_z7_3, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 15, z7, z7, 3),
	    svmls_lane_za32_vg2x1 (w8 + 15, z7, z7, 3))

/*
** mls_lane_w8p16_z7_z7_4:
**	add	(w8|w9|w10|w11), w8, #?16
**	umlsl	za\.s\[\1, 0:1\], z7\.h, z7\.h\[4\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p16_z7_z7_4, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 + 16, z7, z7, 4),
	    svmls_lane_za32_vg2x1 (w8 + 16, z7, z7, 4))

/*
** mls_lane_w8m1_z16_z0_5:
**	sub	(w8|w9|w10|w11), w8, #?1
**	umlsl	za\.s\[\1, 0:1\], z16\.h, z0\.h\[5\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8m1_z16_z0_5, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w8 - 1, z16, z0, 5),
	    svmls_lane_za32_vg2x1 (w8 - 1, z16, z0, 5))

/*
** mls_lane_w12_z0_z3_6:
**	mov	(w8|w9|w10|w11), w12
**	umlsl	za\.s\[\1, 0:1\], z0\.h, z3\.h\[6\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w12_z0_z3_6, svuint16_t,
	    svmls_lane_za32_u16_vg2x1 (w12, z0, z3, 6),
	    svmls_lane_za32_vg2x1 (w12, z0, z3, 6))
