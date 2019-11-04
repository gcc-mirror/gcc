/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st4_f64_base:
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_base, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0, z0),
	    svst4 (p0, x0, z0))

/*
** st4_f64_index:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, x1, lsl 3\]
**	ret
*/
TEST_STORE (st4_f64_index, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + x1, z0),
	    svst4 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_1:
**	incb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_1, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd (), z0),
	    svst4 (p0, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_2:
**	incb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_2, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd () * 2, z0),
	    svst4 (p0, x0 + svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_3:
**	incb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_3, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd () * 3, z0),
	    svst4 (p0, x0 + svcntd () * 3, z0))

/*
** st4_f64_4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_f64_4, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd () * 4, z0),
	    svst4 (p0, x0 + svcntd () * 4, z0))

/*
** st4_f64_28:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_f64_28, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd () * 28, z0),
	    svst4 (p0, x0 + svcntd () * 28, z0))

/*
** st4_f64_32:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_f64_32, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 + svcntd () * 32, z0),
	    svst4 (p0, x0 + svcntd () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_m1:
**	decb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_m1, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd (), z0),
	    svst4 (p0, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_m2:
**	decb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_m2, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd () * 2, z0),
	    svst4 (p0, x0 - svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_f64_m3:
**	decb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_f64_m3, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd () * 3, z0),
	    svst4 (p0, x0 - svcntd () * 3, z0))

/*
** st4_f64_m4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_f64_m4, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd () * 4, z0),
	    svst4 (p0, x0 - svcntd () * 4, z0))

/*
** st4_f64_m32:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_f64_m32, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd () * 32, z0),
	    svst4 (p0, x0 - svcntd () * 32, z0))

/*
** st4_f64_m36:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_f64_m36, svfloat64x4_t, float64_t,
	    svst4_f64 (p0, x0 - svcntd () * 36, z0),
	    svst4 (p0, x0 - svcntd () * 36, z0))

/*
** st4_vnum_f64_0:
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_0, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 0, z0),
	    svst4_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_1:
**	incb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_1, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 1, z0),
	    svst4_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_2:
**	incb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_2, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 2, z0),
	    svst4_vnum (p0, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_3:
**	incb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_3, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 3, z0),
	    svst4_vnum (p0, x0, 3, z0))

/*
** st4_vnum_f64_4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_f64_4, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 4, z0),
	    svst4_vnum (p0, x0, 4, z0))

/*
** st4_vnum_f64_28:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_f64_28, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 28, z0),
	    svst4_vnum (p0, x0, 28, z0))

/*
** st4_vnum_f64_32:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_f64_32, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, 32, z0),
	    svst4_vnum (p0, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_m1:
**	decb	x0
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m1, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -1, z0),
	    svst4_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_m2:
**	decb	x0, all, mul #2
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m2, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -2, z0),
	    svst4_vnum (p0, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st4_vnum_f64_m3:
**	decb	x0, all, mul #3
**	st4d	{z0\.d - z3\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m3, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -3, z0),
	    svst4_vnum (p0, x0, -3, z0))

/*
** st4_vnum_f64_m4:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m4, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -4, z0),
	    svst4_vnum (p0, x0, -4, z0))

/*
** st4_vnum_f64_m32:
**	st4d	{z0\.d - z3\.d}, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m32, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -32, z0),
	    svst4_vnum (p0, x0, -32, z0))

/*
** st4_vnum_f64_m36:
**	[^{]*
**	st4d	{z0\.d - z3\.d}, p0, \[x[0-9]+\]
**	ret
*/
TEST_STORE (st4_vnum_f64_m36, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, -36, z0),
	    svst4_vnum (p0, x0, -36, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st4_vnum_f64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st4d	{z0\.d - z3\.d}, p0, \[\2\]
**	ret
*/
TEST_STORE (st4_vnum_f64_x1, svfloat64x4_t, float64_t,
	    svst4_vnum_f64 (p0, x0, x1, z0),
	    svst4_vnum (p0, x0, x1, z0))
