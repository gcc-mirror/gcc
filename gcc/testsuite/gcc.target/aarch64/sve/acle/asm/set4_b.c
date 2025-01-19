/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** set4_b_p8_0:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	p8\.b, p0\.b
**	ret
*/
TEST_SET_B (set4_b_p8_0, svboolx4_t,
	    p8 = svset4_b (p4, 0, p0),
	    p8 = svset4 (p4, 0, p0))

/*
** set4_b_p8_1:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	p9\.b, p0\.b
**	ret
*/
TEST_SET_B (set4_b_p8_1, svboolx4_t,
	    p8 = svset4_b (p4, 1, p0),
	    p8 = svset4 (p4, 1, p0))

/*
** set4_b_p8_2:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	p10\.b, p0\.b
**	ret
*/
TEST_SET_B (set4_b_p8_2, svboolx4_t,
	    p8 = svset4_b (p4, 2, p0),
	    p8 = svset4 (p4, 2, p0))

/*
** set4_b_p8_3:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	p11\.b, p0\.b
**	ret
*/
TEST_SET_B (set4_b_p8_3, svboolx4_t,
	    p8 = svset4_b (p4, 3, p0),
	    p8 = svset4 (p4, 3, p0))

/*
** set4_b_p4_0:
**	mov	p4\.b, p12\.b
**	ret
*/
TEST_SET_B (set4_b_p4_0, svboolx4_t,
	    p4 = svset4_b (p4, 0, p12),
	    p4 = svset4 (p4, 0, p12))

/*
** set4_b_p4_1:
**	mov	p5\.b, p13\.b
**	ret
*/
TEST_SET_B (set4_b_p4_1, svboolx4_t,
	    p4 = svset4_b (p4, 1, p13),
	    p4 = svset4 (p4, 1, p13))

/*
** set4_b_p4_2:
**	mov	p6\.b, p12\.b
**	ret
*/
TEST_SET_B (set4_b_p4_2, svboolx4_t,
	    p4 = svset4_b (p4, 2, p12),
	    p4 = svset4 (p4, 2, p12))

/*
** set4_b_p4_3:
**	mov	p7\.b, p13\.b
**	ret
*/
TEST_SET_B (set4_b_p4_3, svboolx4_t,
	    p4 = svset4_b (p4, 3, p13),
	    p4 = svset4 (p4, 3, p13))
