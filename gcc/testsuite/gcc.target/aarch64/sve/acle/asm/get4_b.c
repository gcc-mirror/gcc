/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get4_b_p0_0:
**	mov	p0\.b, p4\.b
**	ret
*/
TEST_GET_B (get4_b_p0_0, svboolx4_t,
	    p0 = svget4_b (p4, 0),
	    p0 = svget4 (p4, 0))

/*
** get4_b_p0_1:
**	mov	p0\.b, p5\.b
**	ret
*/
TEST_GET_B (get4_b_p0_1, svboolx4_t,
	    p0 = svget4_b (p4, 1),
	    p0 = svget4 (p4, 1))

/*
** get4_b_p0_2:
**	mov	p0\.b, p6\.b
**	ret
*/
TEST_GET_B (get4_b_p0_2, svboolx4_t,
	    p0 = svget4_b (p4, 2),
	    p0 = svget4 (p4, 2))

/*
** get4_b_p0_3:
**	mov	p0\.b, p7\.b
**	ret
*/
TEST_GET_B (get4_b_p0_3, svboolx4_t,
	    p0 = svget4_b (p4, 3),
	    p0 = svget4 (p4, 3))

/*
** get4_b_p4_0:
**	ret
*/
TEST_GET_B (get4_b_p4_0, svboolx4_t,
	    p4_res = svget4_b (p4, 0),
	    p4_res = svget4 (p4, 0))

/*
** get4_b_p4_3:
**	mov	p4\.b, p7\.b
**	ret
*/
TEST_GET_B (get4_b_p4_3, svboolx4_t,
	    p4_res = svget4_b (p4, 3),
	    p4_res = svget4 (p4, 3))

/*
** get4_b_p5_0:
**	mov	p5\.b, p4\.b
**	ret
*/
TEST_GET_B (get4_b_p5_0, svboolx4_t,
	    p5_res = svget4_b (p4, 0),
	    p5_res = svget4 (p4, 0))

/*
** get4_b_p5_1:
**	ret
*/
TEST_GET_B (get4_b_p5_1, svboolx4_t,
	    p5_res = svget4_b (p4, 1),
	    p5_res = svget4 (p4, 1))
