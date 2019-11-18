/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"
#include <stdbool.h>

/*
** test_bool_pmore:
**	ptest	p0, p1\.b
**	cset	[wx]0, pmore
**	ret
*/
TEST_PTEST (test_bool_pmore, bool,
	    x0 = svptest_any (p0, p1) & !svptest_last (p0, p1));

/*
** test_bool_plast:
**	ptest	p0, p1\.b
**	cset	[wx]0, plast
**	ret
*/
TEST_PTEST (test_bool_plast, bool,
	    x0 = !svptest_any (p0, p1) | svptest_last (p0, p1));

/*
** test_int_pmore:
**	ptest	p0, p1\.b
**	cset	[wx]0, pmore
**	ret
*/
TEST_PTEST (test_int_pmore, int,
	    x0 = svptest_any (p0, p1) & !svptest_last (p0, p1));

/*
** test_int_plast:
**	ptest	p0, p1\.b
**	cset	[wx]0, plast
**	ret
*/
TEST_PTEST (test_int_plast, int,
	    x0 = !svptest_any (p0, p1) | svptest_last (p0, p1));

/*
** test_int64_t_pmore:
**	ptest	p0, p1\.b
**	cset	[wx]0, pmore
**	ret
*/
TEST_PTEST (test_int64_t_pmore, int64_t,
	    x0 = svptest_any (p0, p1) & !svptest_last (p0, p1));

/*
** test_int64_t_plast:
**	ptest	p0, p1\.b
**	cset	[wx]0, plast
**	ret
*/
TEST_PTEST (test_int64_t_plast, int64_t,
	    x0 = !svptest_any (p0, p1) | svptest_last (p0, p1));

/*
** sel_pmore:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, pmore|x1, x0, plast)
**	ret
*/
TEST_PTEST (sel_pmore, int64_t,
	    x0 = svptest_any (p0, p1) & !svptest_last (p0, p1) ? x0 : x1);

/*
** sel_plast:
**	ptest	p0, p1\.b
**	csel	x0, (x0, x1, plast|x1, x0, pmore)
**	ret
*/
TEST_PTEST (sel_plast, int64_t,
	    x0 = !svptest_any (p0, p1) | svptest_last (p0, p1) ? x0 : x1);
