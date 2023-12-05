/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mla_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfmlal	za\.s\[\1, 0:1\], z0\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_0_z0_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (0, z0, z0),
	    svmla_za32_vg2x1 (0, z0, z0))

/*
** mla_w0_z0_z3:
**	mov	(w8|w9|w10|w11), w0
**	bfmlal	za\.s\[\1, 0:1\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mla_w0_z0_z3, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w0, z0, z3),
	    svmla_za32_vg2x1 (w0, z0, z3))

/*
** mla_w7_z0_z3:
**	mov	(w8|w9|w10|w11), w7
**	bfmlal	za\.s\[\1, 0:1\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mla_w7_z0_z3, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w7, z0, z3),
	    svmla_za32_vg2x1 (w7, z0, z3))

/*
** mla_w8_z7_z3:
**	bfmlal	za\.s\[w8, 0:1\], z7\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mla_w8_z7_z3, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8, z7, z3),
	    svmla_za32_vg2x1 (w8, z7, z3))

/*
** mla_w8_z31_z16:
**	mov	(z[0-7])\.d, z16\.d
**	bfmlal	za\.s\[w8, 0:1\], z31\.h. \1\.h
**	ret
*/
TEST_ZA_X1 (mla_w8_z31_z16, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8, z31, z16),
	    svmla_za32_vg2x1 (w8, z31, z16))

/*
** mla_w8p1_z0_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	bfmlal	za\.s\[\1, 0:1\], z0\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p1_z0_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 1, z0, z0),
	    svmla_za32_vg2x1 (w8 + 1, z0, z0))

/*
** mla_w8p2_z23_z0:
**	bfmlal	za\.s\[w8, 2:3\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p2_z23_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 2, z23, z0),
	    svmla_za32_vg2x1 (w8 + 2, z23, z0))

/*
** mla_w11p6_z23_z0:
**	bfmlal	za\.s\[w11, 6:7\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w11p6_z23_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w11 + 6, z23, z0),
	    svmla_za32_vg2x1 (w11 + 6, z23, z0))

/*
** mla_w8p7_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?7
**	bfmlal	za\.s\[\1, 0:1\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p7_z7_z7, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 7, z7, z7),
	    svmla_za32_vg2x1 (w8 + 7, z7, z7))

/*
** mla_w11p10_z23_z0:
**	bfmlal	za\.s\[w11, 10:11\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w11p10_z23_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w11 + 10, z23, z0),
	    svmla_za32_vg2x1 (w11 + 10, z23, z0))

/*
** mla_w8p14_z23_z0:
**	bfmlal	za\.s\[w8, 14:15\], z23\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p14_z23_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 14, z23, z0),
	    svmla_za32_vg2x1 (w8 + 14, z23, z0))

/*
** mla_w8p15_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?15
**	bfmlal	za\.s\[\1, 0:1\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p15_z7_z7, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 15, z7, z7),
	    svmla_za32_vg2x1 (w8 + 15, z7, z7))

/*
** mla_w8p16_z7_z7:
**	add	(w8|w9|w10|w11), w8, #?16
**	bfmlal	za\.s\[\1, 0:1\], z7\.h, z7\.h
**	ret
*/
TEST_ZA_X1 (mla_w8p16_z7_z7, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 + 16, z7, z7),
	    svmla_za32_vg2x1 (w8 + 16, z7, z7))

/*
** mla_w8m1_z16_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	bfmlal	za\.s\[\1, 0:1\], z16\.h, z0\.h
**	ret
*/
TEST_ZA_X1 (mla_w8m1_z16_z0, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w8 - 1, z16, z0),
	    svmla_za32_vg2x1 (w8 - 1, z16, z0))

/*
** mla_w12_z0_z3:
**	mov	(w8|w9|w10|w11), w12
**	bfmlal	za\.s\[\1, 0:1\], z0\.h, z3\.h
**	ret
*/
TEST_ZA_X1 (mla_w12_z0_z3, svbfloat16_t,
	    svmla_za32_bf16_vg2x1 (w12, z0, z3),
	    svmla_za32_vg2x1 (w12, z0, z3))
