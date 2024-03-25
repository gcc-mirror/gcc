/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** psel_p0_p2_p7_0:
**	mov	[wx](1[2-5]), #?0
**	psel	p0, p2, p7\.s\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p0_p2_p7_0, svcount_t,
	       p0 = svpsel_c32 (p2, p7, 0),
	       p0 = svpsel_c32 (p2, p7, 0))

/*
** psel_p2_p13_p8_w11:
**	mov	[wx](1[2-5]), [wx]11
**	psel	p2, p13, p8\.s\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p2_p13_p8_w11, svcount_t,
	       p2 = svpsel_c32 (p13, p8, w11),
	       p2 = svpsel_c32 (p13, p8, w11))

/*
** psel_p0_p13_p15_w12:
**	psel	p0, p13, p15\.s\[w12, 0\]
**	ret
*/
TEST_SELECT_P (psel_p0_p13_p15_w12, svcount_t,
	       p0 = svpsel_c32 (p13, p15, w12),
	       p0 = svpsel_c32 (p13, p15, w12))

/*
** psel_p2_p0_p15_w15:
**	psel	p2, p0, p15\.s\[w15, 0\]
**	ret
*/
TEST_SELECT_P (psel_p2_p0_p15_w15, svcount_t,
	       p2 = svpsel_c32 (p0, p15, w15),
	       p2 = svpsel_c32 (p0, p15, w15))

/*
** psel_p13_p0_p7_w16:
**	mov	[wx](1[2-5]), [wx]16
**	psel	p13, p0, p7\.s\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p13_p0_p7_w16, svcount_t,
	       p13 = svpsel_c32 (p0, p7, w16),
	       p13 = svpsel_c32 (p0, p7, w16))

/*
** psel_p2_p13_p8_w12p1:
**	psel	p2, p13, p8\.s\[w12, 1\]
**	ret
*/
TEST_SELECT_P (psel_p2_p13_p8_w12p1, svcount_t,
	       p2 = svpsel_c32 (p13, p8, w12 + 1),
	       p2 = svpsel_c32 (p13, p8, w12 + 1))

/*
** psel_p13_p0_p7_w12p3:
**	psel	p13, p0, p7\.s\[w12, 3\]
**	ret
*/
TEST_SELECT_P (psel_p13_p0_p7_w12p3, svcount_t,
	       p13 = svpsel_c32 (p0, p7, w12 + 3),
	       p13 = svpsel_c32 (p0, p7, w12 + 3))

/*
** psel_p0_p0_p7_w12p4:
**	add	(w[0-9]+), w12, #?4
**	psel	p0, p0, p7\.s\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p0_p0_p7_w12p4, svcount_t,
	       p0 = svpsel_c32 (p0, p7, w12 + 4),
	       p0 = svpsel_c32 (p0, p7, w12 + 4))

/*
** psel_p13_p13_p15_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	psel	p13, p13, p15\.s\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_p13_p13_p15_w12m1, svcount_t,
	       p13 = svpsel_c32 (p13, p15, w12 - 1),
	       p13 = svpsel_c32 (p13, p15, w12 - 1))
