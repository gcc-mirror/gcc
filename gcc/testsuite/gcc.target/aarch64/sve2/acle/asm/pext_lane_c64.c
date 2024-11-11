/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** pext_lane_p2_pn0_0:
**	mov	p([0-9]+)\.b, p0\.b
**	pext	p2\.d, pn\1\[0\]
**	ret
*/
TEST_EXTRACT_PN (pext_lane_p2_pn0_0, svbool_t,
		 p2 = svpext_lane_c64 (pn0, 0),
		 p2 = svpext_lane_c64 (pn0, 0))

/*
** pext_lane_p5_pn7_1:
**	mov	p([0-9]+)\.b, p7\.b
**	pext	p5\.d, pn\1\[1\]
**	ret
*/
TEST_EXTRACT_PN (pext_lane_p5_pn7_1, svbool_t,
		 p5 = svpext_lane_c64 (pn7, 1),
		 p5 = svpext_lane_c64 (pn7, 1))

/*
** pext_lane_p9_pn8_2:
**	pext	p9\.d, pn8\[2\]
**	ret
*/
TEST_EXTRACT_PN (pext_lane_p9_pn8_2, svbool_t,
		 p9 = svpext_lane_c64 (pn8, 2),
		 p9 = svpext_lane_c64 (pn8, 2))

/*
** pext_lane_p12_pn11_3:
**	pext	p12\.d, pn11\[3\]
**	ret
*/
TEST_EXTRACT_PN (pext_lane_p12_pn11_3, svbool_t,
		 p12 = svpext_lane_c64 (pn11, 3),
		 p12 = svpext_lane_c64 (pn11, 3))

/*
** pext_lane_p2_pn15_0:
**	pext	p2\.d, pn15\[0\]
**	ret
*/
TEST_EXTRACT_PN (pext_lane_p2_pn15_0, svbool_t,
		 p2 = svpext_lane_c64 (pn15, 0),
		 p2 = svpext_lane_c64 (pn15, 0))
