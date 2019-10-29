/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** test_bool_any:
**	ptest	p0, p1\.b
**	cset	[wx]0, any
**	ret
*/
TEST_PTEST (test_bool_any, bool,
	    x0 = svptest_any (p0, p1));

/*
** test_bool_none:
**	ptest	p0, p1\.b
**	cset	[wx]0, none
**	ret
*/
TEST_PTEST (test_bool_none, bool,
	    x0 = !svptest_any (p0, p1));

/*
** test_int_any:
**	ptest	p0, p1\.b
**	cset	[wx]0, any
**	ret
*/
TEST_PTEST (test_int_any, int,
	    x0 = svptest_any (p0, p1));

/*
** test_int_none:
**	ptest	p0, p1\.b
**	cset	[wx]0, none
**	ret
*/
TEST_PTEST (test_int_none, int,
	    x0 = !svptest_any (p0, p1));

/*
** test_int64_t_any:
**	ptest	p0, p1\.b
**	cset	[wx]0, any
**	ret
*/
TEST_PTEST (test_int64_t_any, int64_t,
	    x0 = svptest_any (p0, p1));

/*
** test_int64_t_none:
**	ptest	p0, p1\.b
**	cset	[wx]0, none
**	ret
*/
TEST_PTEST (test_int64_t_none, int64_t,
	    x0 = !svptest_any (p0, p1));

/*
** sel_any:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, any|x1, x0, none)
**	ret
*/
TEST_PTEST (sel_any, int64_t,
	    x0 = svptest_any (p0, p1) ? x0 : x1);

/*
** sel_none:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, none|x1, x0, any)
**	ret
*/
TEST_PTEST (sel_none, int64_t,
	    x0 = !svptest_any (p0, p1) ? x0 : x1);
