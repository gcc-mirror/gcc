/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_b_tied1:
**	sel	p0\.b, p3, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (sel_b_tied1,
		p0 = svsel_b (p3, p0, p1),
		p0 = svsel (p3, p0, p1))

/*
** sel_b_tied2:
**	sel	p0\.b, p3, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (sel_b_tied2,
		p0 = svsel_b (p3, p1, p0),
		p0 = svsel (p3, p1, p0))

/*
** sel_b_untied:
**	sel	p0\.b, p3, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (sel_b_untied,
		p0 = svsel_b (p3, p1, p2),
		p0 = svsel (p3, p1, p2))
