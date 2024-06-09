/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_lane_0_z0_z0_0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsll	za\.s\[\1, 0:3\], z0\.b, z0\.b\[0\]
**	ret
*/
TEST_ZA_X1 (mls_lane_0_z0_z0_0, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (0, z0, z0, 0),
	    svmls_lane_za32_vg4x1 (0, z0, z0, 0))

/*
** mls_lane_w0_z0_z3_1:
**	mov	(w8|w9|w10|w11), w0
**	umlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b\[1\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w0_z0_z3_1, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w0, z0, z3, 1),
	    svmls_lane_za32_vg4x1 (w0, z0, z3, 1))

/*
** mls_lane_w7_z0_z3_2:
**	mov	(w8|w9|w10|w11), w7
**	umlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b\[2\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w7_z0_z3_2, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w7, z0, z3, 2),
	    svmls_lane_za32_vg4x1 (w7, z0, z3, 2))

/*
** mls_lane_w8_z7_z3_3:
**	umlsll	za\.s\[w8, 0:3\], z7\.b, z3\.b\[3\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8_z7_z3_3, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8, z7, z3, 3),
	    svmls_lane_za32_vg4x1 (w8, z7, z3, 3))

/*
** mls_lane_w8_z31_z16_4:
**	mov	(z[0-7])\.d, z16\.d
**	umlsll	za\.s\[w8, 0:3\], z31\.b. \1\.b\[4\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8_z31_z16_4, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8, z31, z16, 4),
	    svmls_lane_za32_vg4x1 (w8, z31, z16, 4))

/*
** mls_lane_w8p1_z0_z0_5:
**	add	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.s\[\1, 0:3\], z0\.b, z0\.b\[5\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p1_z0_z0_5, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 1, z0, z0, 5),
	    svmls_lane_za32_vg4x1 (w8 + 1, z0, z0, 5))

/*
** mls_lane_w8p2_z23_z0_6:
**	add	(w8|w9|w10|w11), w8, #?2
**	umlsll	za\.s\[\1, 0:3\], z23\.b, z0\.b\[6\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p2_z23_z0_6, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 2, z23, z0, 6),
	    svmls_lane_za32_vg4x1 (w8 + 2, z23, z0, 6))

/*
** mls_lane_w11p4_z23_z0_7:
**	umlsll	za\.s\[w11, 4:7\], z23\.b, z0\.b\[7\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w11p4_z23_z0_7, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w11 + 4, z23, z0, 7),
	    svmls_lane_za32_vg4x1 (w11 + 4, z23, z0, 7))

/*
** mls_lane_w8p7_z7_z7_8:
**	add	(w8|w9|w10|w11), w8, #?7
**	umlsll	za\.s\[\1, 0:3\], z7\.b, z7\.b\[8\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p7_z7_z7_8, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 7, z7, z7, 8),
	    svmls_lane_za32_vg4x1 (w8 + 7, z7, z7, 8))

/*
** mls_lane_w11p12_z23_z0_9:
**	umlsll	za\.s\[w11, 12:15\], z23\.b, z0\.b\[9\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w11p12_z23_z0_9, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w11 + 12, z23, z0, 9),
	    svmls_lane_za32_vg4x1 (w11 + 12, z23, z0, 9))

/*
** mls_lane_w8p14_z23_z0_10:
**	add	(w8|w9|w10|w11), w8, #?14
**	umlsll	za\.s\[w8, 0:3\], z23\.b, z0\.b\[10\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p14_z23_z0_10, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 14, z23, z0, 10),
	    svmls_lane_za32_vg4x1 (w8 + 14, z23, z0, 10))

/*
** mls_lane_w8p15_z7_z7_11:
**	add	(w8|w9|w10|w11), w8, #?15
**	umlsll	za\.s\[\1, 0:3\], z7\.b, z7\.b\[11\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p15_z7_z7_11, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 15, z7, z7, 11),
	    svmls_lane_za32_vg4x1 (w8 + 15, z7, z7, 11))

/*
** mls_lane_w8p16_z7_z7_12:
**	add	(w8|w9|w10|w11), w8, #?16
**	umlsll	za\.s\[\1, 0:3\], z7\.b, z7\.b\[12\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8p16_z7_z7_12, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 + 16, z7, z7, 12),
	    svmls_lane_za32_vg4x1 (w8 + 16, z7, z7, 12))

/*
** mls_lane_w8m1_z16_z0_13:
**	sub	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.s\[\1, 0:3\], z16\.b, z0\.b\[13\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w8m1_z16_z0_13, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w8 - 1, z16, z0, 13),
	    svmls_lane_za32_vg4x1 (w8 - 1, z16, z0, 13))

/*
** mls_lane_w12_z0_z3_15:
**	mov	(w8|w9|w10|w11), w12
**	umlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b\[15\]
**	ret
*/
TEST_ZA_X1 (mls_lane_w12_z0_z3_15, svuint8_t,
	    svmls_lane_za32_u8_vg4x1 (w12, z0, z3, 15),
	    svmls_lane_za32_vg4x1 (w12, z0, z3, 15))
