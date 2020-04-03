/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st4_s64_base:
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_base, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0, z0),
	    svst4 (p0, x0, z0))

/*
** st4_s64_index:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, x1, lsl 3\]
**	ret
*/
TEST_STORE (st4_s64_index, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + x1, z0),
	    svst4 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_1:
**	incb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_1, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd (), z0),
	    svst4 (p0, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_2:
**	incb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_2, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd () * 2, z0),
	    svst4 (p0, x0 + svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_3:
**	incb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_3, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd () * 3, z0),
	    svst4 (p0, x0 + svcntd () * 3, z0))

/*
** st4_s64_4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_s64_4, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd () * 4, z0),
	    svst4 (p0, x0 + svcntd () * 4, z0))

/*
** st4_s64_28:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_s64_28, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd () * 28, z0),
	    svst4 (p0, x0 + svcntd () * 28, z0))

/*
** st4_s64_32:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_s64_32, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 + svcntd () * 32, z0),
	    svst4 (p0, x0 + svcntd () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_m1:
**	decb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_m1, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd (), z0),
	    svst4 (p0, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_m2:
**	decb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_m2, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd () * 2, z0),
	    svst4 (p0, x0 - svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_s64_m3:
**	decb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_s64_m3, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd () * 3, z0),
	    svst4 (p0, x0 - svcntd () * 3, z0))

/*
** st4_s64_m4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_s64_m4, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd () * 4, z0),
	    svst4 (p0, x0 - svcntd () * 4, z0))

/*
** st4_s64_m32:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_s64_m32, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd () * 32, z0),
	    svst4 (p0, x0 - svcntd () * 32, z0))

/*
** st4_s64_m36:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_s64_m36, svint64x4_t, int64_t,
	    svst4_s64 (p0, x0 - svcntd () * 36, z0),
	    svst4 (p0, x0 - svcntd () * 36, z0))

/*
** st4_vnum_s64_0:
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_0, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 0, z0),
	    svst4_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_1:
**	incb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_1, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 1, z0),
	    svst4_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_2:
**	incb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_2, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 2, z0),
	    svst4_vnum (p0, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_3:
**	incb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_3, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 3, z0),
	    svst4_vnum (p0, x0, 3, z0))

/*
** st4_vnum_s64_4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_s64_4, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 4, z0),
	    svst4_vnum (p0, x0, 4, z0))

/*
** st4_vnum_s64_28:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_s64_28, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 28, z0),
	    svst4_vnum (p0, x0, 28, z0))

/*
** st4_vnum_s64_32:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_s64_32, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, 32, z0),
	    svst4_vnum (p0, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_m1:
**	decb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m1, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -1, z0),
	    svst4_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_m2:
**	decb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m2, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -2, z0),
	    svst4_vnum (p0, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_s64_m3:
**	decb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m3, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -3, z0),
	    svst4_vnum (p0, x0, -3, z0))

/*
** st4_vnum_s64_m4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m4, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -4, z0),
	    svst4_vnum (p0, x0, -4, z0))

/*
** st4_vnum_s64_m32:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m32, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -32, z0),
	    svst4_vnum (p0, x0, -32, z0))

/*
** st4_vnum_s64_m36:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_s64_m36, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, -36, z0),
	    svst4_vnum (p0, x0, -36, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st4_vnum_s64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st4d	{z0\.d - z3\.d}, p0, \[\2\]
**	ret
*/
TEST_STORE (st4_vnum_s64_x1, svint64x4_t, int64_t,
	    svst4_vnum_s64 (p0, x0, x1, z0),
	    svst4_vnum (p0, x0, x1, z0))
