/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** psel_lane_p0_p2_p7_0:
**	mov	[wx](1[2-5]), #?0
**	psel	p0, p2, p7\.d\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p0_p2_p7_0, svcount_t,
	       p0 = svpsel_lane_c64 (p2, p7, 0),
	       p0 = svpsel_lane_c64 (p2, p7, 0))

/*
** psel_lane_p2_p13_p8_w11:
**	mov	[wx](1[2-5]), [wx]11
**	psel	p2, p13, p8\.d\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p2_p13_p8_w11, svcount_t,
	       p2 = svpsel_lane_c64 (p13, p8, w11),
	       p2 = svpsel_lane_c64 (p13, p8, w11))

/*
** psel_lane_p2_p0_p15_w12:
**	psel	p2, p0, p15\.d\[w12, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p2_p0_p15_w12, svcount_t,
	       p2 = svpsel_lane_c64 (p0, p15, w12),
	       p2 = svpsel_lane_c64 (p0, p15, w12))

/*
** psel_lane_p0_p13_p15_w15:
**	psel	p0, p13, p15\.d\[w15, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p0_p13_p15_w15, svcount_t,
	       p0 = svpsel_lane_c64 (p13, p15, w15),
	       p0 = svpsel_lane_c64 (p13, p15, w15))

/*
** psel_lane_p13_p0_p15_w16:
**	mov	[wx](1[2-5]), [wx]16
**	psel	p13, p0, p15\.d\[w\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p13_p0_p15_w16, svcount_t,
	       p13 = svpsel_lane_c64 (p0, p15, w16),
	       p13 = svpsel_lane_c64 (p0, p15, w16))

/*
** psel_lane_p2_p13_p8_w12p1:
**	psel	p2, p13, p8\.d\[w12, 1\]
**	ret
*/
TEST_SELECT_P (psel_lane_p2_p13_p8_w12p1, svcount_t,
	       p2 = svpsel_lane_c64 (p13, p8, w12 + 1),
	       p2 = svpsel_lane_c64 (p13, p8, w12 + 1))

/*
** psel_lane_p0_p0_p8_w12p2:
**	add	(w[0-9]+), w12, #?2
**	psel	p0, p0, p8\.d\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p0_p0_p8_w12p2, svcount_t,
	       p0 = svpsel_lane_c64 (p0, p8, w12 + 2),
	       p0 = svpsel_lane_c64 (p0, p8, w12 + 2))

/*
** psel_lane_p13_p13_p15_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	psel	p13, p13, p15\.d\[\1, 0\]
**	ret
*/
TEST_SELECT_P (psel_lane_p13_p13_p15_w12m1, svcount_t,
	       p13 = svpsel_lane_c64 (p13, p15, w12 - 1),
	       p13 = svpsel_lane_c64 (p13, p15, w12 - 1))
