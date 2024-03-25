/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set2_b_p8_0:
**	mov	p9\.b, p5\.b
**	mov	p8\.b, p0\.b
**	ret
*/
TEST_SET_B (set2_b_p8_0, svboolx2_t,
	    p8 = svset2_b (p4, 0, p0),
	    p8 = svset2 (p4, 0, p0))

/*
** set2_b_p8_1:
**	mov	p8\.b, p4\.b
**	mov	p9\.b, p0\.b
**	ret
*/
TEST_SET_B (set2_b_p8_1, svboolx2_t,
	    p8 = svset2_b (p4, 1, p0),
	    p8 = svset2 (p4, 1, p0))

/*
** set2_b_p4_0:
**	mov	p4\.b, p12\.b
**	ret
*/
TEST_SET_B (set2_b_p4_0, svboolx2_t,
	    p4 = svset2_b (p4, 0, p12),
	    p4 = svset2 (p4, 0, p12))

/*
** set2_b_p4_1:
**	mov	p5\.b, p13\.b
**	ret
*/
TEST_SET_B (set2_b_p4_1, svboolx2_t,
	    p4 = svset2_b (p4, 1, p13),
	    p4 = svset2 (p4, 1, p13))
