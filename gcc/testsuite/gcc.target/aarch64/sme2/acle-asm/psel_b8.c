/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** psel_p0_p2_p7_0:
**	mov	[wx](1[2-5]), #?0
**	psel	p0, p2, p7\.b\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p0_p2_p7_0, svbool_t,
	       p0 = svpsel_b8 (p2, p7, 0),
	       p0 = svpsel_b8 (p2, p7, 0))

/*
** psel_p2_p7_p8_w11:
**	mov	[wx](1[2-5]), [wx]11
**	psel	p2, p7, p8\.b\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p2_p7_p8_w11, svbool_t,
	       p2 = svpsel_b8 (p7, p8, w11),
	       p2 = svpsel_b8 (p7, p8, w11))

/*
** psel_p7_p8_p13_w12:
**	psel	p7, p8, p13\.b\[w12, 0\]
**	ret
*/
TEST_SELECT_P (psel_p7_p8_p13_w12, svbool_t,
	       p7 = svpsel_b8 (p8, p13, w12),
	       p7 = svpsel_b8 (p8, p13, w12))

/*
** psel_p8_p13_p15_w15:
**	psel	p8, p13, p15\.b\[w15, 0\]
**	ret
*/
TEST_SELECT_P (psel_p8_p13_p15_w15, svbool_t,
	       p8 = svpsel_b8 (p13, p15, w15),
	       p8 = svpsel_b8 (p13, p15, w15))

/*
** psel_p13_p15_p0_w16:
**	mov	[wx](1[2-5]), [wx]16
**	psel	p13, p15, p0\.b\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p13_p15_p0_w16, svbool_t,
	       p13 = svpsel_b8 (p15, p0, w16),
	       p13 = svpsel_b8 (p15, p0, w16))

/*
** psel_p15_p13_p8_w12p1:
**	psel	p15, p13, p8\.b\[w12, 1\]
**	ret
*/
TEST_SELECT_P (psel_p15_p13_p8_w12p1, svbool_t,
	       p15 = svpsel_b8 (p13, p8, w12 + 1),
	       p15 = svpsel_b8 (p13, p8, w12 + 1))

/*
** psel_p13_p8_p7_w12p15:
**	psel	p13, p8, p7\.b\[w12, 15\]
**	ret
*/
TEST_SELECT_P (psel_p13_p8_p7_w12p15, svbool_t,
	       p13 = svpsel_b8 (p8, p7, w12 + 15),
	       p13 = svpsel_b8 (p8, p7, w12 + 15))

/*
** psel_p0_p0_p0_w12p16:
**	add	(w[0-9]+), w12, #?16
**	psel	p0, p0, p0\.b\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p0_p0_p0_w12p16, svbool_t,
	       p0 = svpsel_b8 (p0, p0, w12 + 16),
	       p0 = svpsel_b8 (p0, p0, w12 + 16))

/*
** psel_p15_p15_p15_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	psel	p15, p15, p15\.b\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p15_p15_p15_w12m1, svbool_t,
	       p15 = svpsel_b8 (p15, p15, w12 - 1),
	       p15 = svpsel_b8 (p15, p15, w12 - 1))
