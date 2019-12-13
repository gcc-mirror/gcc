/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** test_bool_first:
**	ptest	p0, p1\.b
**	cset	[wx]0, first
**	ret
*/
TEST_PTEST (test_bool_first, bool,
	    x0 = svptest_first (p0, p1));

/*
** test_bool_nfrst:
**	ptest	p0, p1\.b
**	cset	[wx]0, nfrst
**	ret
*/
TEST_PTEST (test_bool_nfrst, bool,
	    x0 = !svptest_first (p0, p1));

/*
** test_int_first:
**	ptest	p0, p1\.b
**	cset	[wx]0, first
**	ret
*/
TEST_PTEST (test_int_first, int,
	    x0 = svptest_first (p0, p1));

/*
** test_int_nfrst:
**	ptest	p0, p1\.b
**	cset	[wx]0, nfrst
**	ret
*/
TEST_PTEST (test_int_nfrst, int,
	    x0 = !svptest_first (p0, p1));

/*
** test_int64_t_first:
**	ptest	p0, p1\.b
**	cset	[wx]0, first
**	ret
*/
TEST_PTEST (test_int64_t_first, int64_t,
	    x0 = svptest_first (p0, p1));

/*
** test_int64_t_nfrst:
**	ptest	p0, p1\.b
**	cset	[wx]0, nfrst
**	ret
*/
TEST_PTEST (test_int64_t_nfrst, int64_t,
	    x0 = !svptest_first (p0, p1));

/*
** sel_first:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, first|x1, x0, nfrst)
**	ret
*/
TEST_PTEST (sel_first, int64_t,
	    x0 = svptest_first (p0, p1) ? x0 : x1);

/*
** sel_nfrst:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, nfrst|x1, x0, first)
**	ret
*/
TEST_PTEST (sel_nfrst, int64_t,
	    x0 = !svptest_first (p0, p1) ? x0 : x1);
