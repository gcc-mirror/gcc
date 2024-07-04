/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get2_b_p0_0:
**	mov	p0\.b, p4\.b
**	ret
*/
TEST_GET_B (get2_b_p0_0, svboolx2_t,
	    p0 = svget2_b (p4, 0),
	    p0 = svget2 (p4, 0))

/*
** get2_b_p0_1:
**	mov	p0\.b, p5\.b
**	ret
*/
TEST_GET_B (get2_b_p0_1, svboolx2_t,
	    p0 = svget2_b (p4, 1),
	    p0 = svget2 (p4, 1))

/*
** get2_b_p4_0:
**	ret
*/
TEST_GET_B (get2_b_p4_0, svboolx2_t,
	    p4_res = svget2_b (p4, 0),
	    p4_res = svget2 (p4, 0))

/*
** get2_b_p4_1:
**	mov	p4\.b, p5\.b
**	ret
*/
TEST_GET_B (get2_b_p4_1, svboolx2_t,
	    p4_res = svget2_b (p4, 1),
	    p4_res = svget2 (p4, 1))

/*
** get2_b_p5_0:
**	mov	p5\.b, p4\.b
**	ret
*/
TEST_GET_B (get2_b_p5_0, svboolx2_t,
	    p5_res = svget2_b (p4, 0),
	    p5_res = svget2 (p4, 0))

/*
** get2_b_p5_1:
**	ret
*/
TEST_GET_B (get2_b_p5_1, svboolx2_t,
	    p5_res = svget2_b (p4, 1),
	    p5_res = svget2 (p4, 1))
