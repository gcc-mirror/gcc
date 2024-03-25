/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	smlsll	za\.s\[\1, 0:3\], z0\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_0_z0_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (0, z0, z0),
	    svmls_za32_vg4x1 (0, z0, z0))

/*
** mls_w0_z0_z3:
**	mov	(w8|w9|w10|w11), w0
**	smlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mls_w0_z0_z3, svint8_t,
	    svmls_za32_s8_vg4x1 (w0, z0, z3),
	    svmls_za32_vg4x1 (w0, z0, z3))

/*
** mls_w7_z0_z3:
**	mov	(w8|w9|w10|w11), w7
**	smlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mls_w7_z0_z3, svint8_t,
	    svmls_za32_s8_vg4x1 (w7, z0, z3),
	    svmls_za32_vg4x1 (w7, z0, z3))

/*
** mls_w8_z7_z3:
**	smlsll	za\.s\[w8, 0:3\], z7\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mls_w8_z7_z3, svint8_t,
	    svmls_za32_s8_vg4x1 (w8, z7, z3),
	    svmls_za32_vg4x1 (w8, z7, z3))

/*
** mls_w8_z31_z16:
**	mov	(z[0-7])\.d, z16\.d
**	smlsll	za\.s\[w8, 0:3\], z31\.b. \1\.b
**	ret
*/
TEST_ZA_X1 (mls_w8_z31_z16, svint8_t,
	    svmls_za32_s8_vg4x1 (w8, z31, z16),
	    svmls_za32_vg4x1 (w8, z31, z16))

/*
** mls_w8p1_z0_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	smlsll	za\.s\[\1, 0:3\], z0\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w8p1_z0_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w8 + 1, z0, z0),
	    svmls_za32_vg4x1 (w8 + 1, z0, z0))

/*
** mls_w10p4_z23_z0:
**	smlsll	za\.s\[w10, 4:7\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w10p4_z23_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w10 + 4, z23, z0),
	    svmls_za32_vg4x1 (w10 + 4, z23, z0))

/*
** mls_w11p6_z23_z0:
**	add	(w8|w9|w10|w11), w11, #?6
**	smlsll	za\.s\[\1, 0:3\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w11p6_z23_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w11 + 6, z23, z0),
	    svmls_za32_vg4x1 (w11 + 6, z23, z0))

/*
** mls_w9p8_z7_z7:
**	smlsll	za\.s\[w9, 8:11\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mls_w9p8_z7_z7, svint8_t,
	    svmls_za32_s8_vg4x1 (w9 + 8, z7, z7),
	    svmls_za32_vg4x1 (w9 + 8, z7, z7))

/*
** mls_w11p12_z23_z0:
**	smlsll	za\.s\[w11, 12:15\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w11p12_z23_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w11 + 12, z23, z0),
	    svmls_za32_vg4x1 (w11 + 12, z23, z0))

/*
** mls_w8p14_z23_z0:
**	add	(w8|w9|w10|w11), w8, #?14
**	smlsll	za\.s\[\1, 0:3\], z23\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w8p14_z23_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w8 + 14, z23, z0),
	    svmls_za32_vg4x1 (w8 + 14, z23, z0))

/*
** mls_w8p15_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?15
**	smlsll	za\.s\[\1, 0:3\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mls_w8p15_z7_z7, svint8_t,
	    svmls_za32_s8_vg4x1 (w8 + 15, z7, z7),
	    svmls_za32_vg4x1 (w8 + 15, z7, z7))

/*
** mls_w8p16_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?16
**	smlsll	za\.s\[\1, 0:3\], z7\.b, z7\.b
**	ret
*/
TEST_ZA_X1 (mls_w8p16_z7_z7, svint8_t,
	    svmls_za32_s8_vg4x1 (w8 + 16, z7, z7),
	    svmls_za32_vg4x1 (w8 + 16, z7, z7))

/*
** mls_w8m1_z16_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	smlsll	za\.s\[\1, 0:3\], z16\.b, z0\.b
**	ret
*/
TEST_ZA_X1 (mls_w8m1_z16_z0, svint8_t,
	    svmls_za32_s8_vg4x1 (w8 - 1, z16, z0),
	    svmls_za32_vg4x1 (w8 - 1, z16, z0))

/*
** mls_w12_z0_z3:
**	mov	(w8|w9|w10|w11), w12
**	smlsll	za\.s\[\1, 0:3\], z0\.b, z3\.b
**	ret
*/
TEST_ZA_X1 (mls_w12_z0_z3, svint8_t,
	    svmls_za32_s8_vg4x1 (w12, z0, z3),
	    svmls_za32_vg4x1 (w12, z0, z3))
