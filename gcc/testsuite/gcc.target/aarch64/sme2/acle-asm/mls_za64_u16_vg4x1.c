/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** mls_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsll	za\.d\[\1, 0:3\], z0\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_0_z0_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (0, z0, z0),
	    svmls_za64_vg4x1 (0, z0, z0))

/*
** mls_w0_z0_z3:
**	mov	(w8|w9|w10|w11), w0
**	umlsll	za\.d\[\1, 0:3\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mls_w0_z0_z3, svuint16_t,
	    svmls_za64_u16_vg4x1 (w0, z0, z3),
	    svmls_za64_vg4x1 (w0, z0, z3))

/*
** mls_w7_z0_z3:
**	mov	(w8|w9|w10|w11), w7
**	umlsll	za\.d\[\1, 0:3\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mls_w7_z0_z3, svuint16_t,
	    svmls_za64_u16_vg4x1 (w7, z0, z3),
	    svmls_za64_vg4x1 (w7, z0, z3))

/*
** mls_w8_z7_z3:
**	umlsll	za\.d\[w8, 0:3\], z7\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mls_w8_z7_z3, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8, z7, z3),
	    svmls_za64_vg4x1 (w8, z7, z3))

/*
** mls_w8_z31_z16:
**	mov	(z[0-7])\.d, z16\.d
**	umlsll	za\.d\[w8, 0:3\], z31\.h. \1\.h
**	ret
*/
TEST_ZA_X1 (mls_w8_z31_z16, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8, z31, z16),
	    svmls_za64_vg4x1 (w8, z31, z16))

/*
** mls_w8p1_z0_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.d\[\1, 0:3\], z0\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w8p1_z0_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8 + 1, z0, z0),
	    svmls_za64_vg4x1 (w8 + 1, z0, z0))

/*
** mls_w10p4_z23_z0:
**	umlsll	za\.d\[w10, 4:7\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w10p4_z23_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w10 + 4, z23, z0),
	    svmls_za64_vg4x1 (w10 + 4, z23, z0))

/*
** mls_w11p6_z23_z0:
**	add	(w8|w9|w10|w11), w11, #?6
**	umlsll	za\.d\[\1, 0:3\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w11p6_z23_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w11 + 6, z23, z0),
	    svmls_za64_vg4x1 (w11 + 6, z23, z0))

/*
** mls_w9p8_z7_z7:
**	umlsll	za\.d\[w9, 8:11\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mls_w9p8_z7_z7, svuint16_t,
	    svmls_za64_u16_vg4x1 (w9 + 8, z7, z7),
	    svmls_za64_vg4x1 (w9 + 8, z7, z7))

/*
** mls_w11p12_z23_z0:
**	umlsll	za\.d\[w11, 12:15\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w11p12_z23_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w11 + 12, z23, z0),
	    svmls_za64_vg4x1 (w11 + 12, z23, z0))

/*
** mls_w8p14_z23_z0:
**	add	(w8|w9|w10|w11), w8, #?14
**	umlsll	za\.d\[\1, 0:3\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w8p14_z23_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8 + 14, z23, z0),
	    svmls_za64_vg4x1 (w8 + 14, z23, z0))

/*
** mls_w8p15_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?15
**	umlsll	za\.d\[\1, 0:3\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mls_w8p15_z7_z7, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8 + 15, z7, z7),
	    svmls_za64_vg4x1 (w8 + 15, z7, z7))

/*
** mls_w8p16_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?16
**	umlsll	za\.d\[\1, 0:3\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mls_w8p16_z7_z7, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8 + 16, z7, z7),
	    svmls_za64_vg4x1 (w8 + 16, z7, z7))

/*
** mls_w8m1_z16_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.d\[\1, 0:3\], z16\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mls_w8m1_z16_z0, svuint16_t,
	    svmls_za64_u16_vg4x1 (w8 - 1, z16, z0),
	    svmls_za64_vg4x1 (w8 - 1, z16, z0))

/*
** mls_w12_z0_z3:
**	mov	(w8|w9|w10|w11), w12
**	umlsll	za\.d\[\1, 0:3\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mls_w12_z0_z3, svuint16_t,
	    svmls_za64_u16_vg4x1 (w12, z0, z3),
	    svmls_za64_vg4x1 (w12, z0, z3))
