/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** test_bool_last:
**	ptest	p0, p1\.b
**	cset	[wx]0, last
**	ret
*/
TEST_PTEST (test_bool_last, bool,
	    x0 = svptest_last (p0, p1));

/*
** test_bool_nlast:
**	ptest	p0, p1\.b
**	cset	[wx]0, nlast
**	ret
*/
TEST_PTEST (test_bool_nlast, bool,
	    x0 = !svptest_last (p0, p1));

/*
** test_int_last:
**	ptest	p0, p1\.b
**	cset	[wx]0, last
**	ret
*/
TEST_PTEST (test_int_last, int,
	    x0 = svptest_last (p0, p1));

/*
** test_int_nlast:
**	ptest	p0, p1\.b
**	cset	[wx]0, nlast
**	ret
*/
TEST_PTEST (test_int_nlast, int,
	    x0 = !svptest_last (p0, p1));

/*
** test_int64_t_last:
**	ptest	p0, p1\.b
**	cset	[wx]0, last
**	ret
*/
TEST_PTEST (test_int64_t_last, int64_t,
	    x0 = svptest_last (p0, p1));

/*
** test_int64_t_nlast:
**	ptest	p0, p1\.b
**	cset	[wx]0, nlast
**	ret
*/
TEST_PTEST (test_int64_t_nlast, int64_t,
	    x0 = !svptest_last (p0, p1));

/*
** sel_last:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, last|x1, x0, nlast)
**	ret
*/
TEST_PTEST (sel_last, int64_t,
	    x0 = svptest_last (p0, p1) ? x0 : x1);

/*
** sel_nlast:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, nlast|x1, x0, last)
**	ret
*/
TEST_PTEST (sel_nlast, int64_t,
	    x0 = !svptest_last (p0, p1) ? x0 : x1);
