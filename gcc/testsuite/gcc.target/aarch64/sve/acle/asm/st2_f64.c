/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st2_f64_base:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_f64_base, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0, z0),
	    svst2 (p0, x0, z0))

/*
** st2_f64_index:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, x1, lsl 3\]
**	ret
*/
TEST_STORE (st2_f64_index, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 + x1, z0),
	    svst2 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_f64_1:
**	incb	x0
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_f64_1, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 + svcntd (), z0),
	    svst2 (p0, x0 + svcntd (), z0))

/*
** st2_f64_2:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_f64_2, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 + svcntd () * 2, z0),
	    svst2 (p0, x0 + svcntd () * 2, z0))

/*
** st2_f64_14:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_f64_14, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 + svcntd () * 14, z0),
	    svst2 (p0, x0 + svcntd () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_f64_16:
**	incb	x0, all, mul #16
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_f64_16, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 + svcntd () * 16, z0),
	    svst2 (p0, x0 + svcntd () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_f64_m1:
**	decb	x0
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_f64_m1, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 - svcntd (), z0),
	    svst2 (p0, x0 - svcntd (), z0))

/*
** st2_f64_m2:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_f64_m2, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 - svcntd () * 2, z0),
	    svst2 (p0, x0 - svcntd () * 2, z0))

/*
** st2_f64_m16:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_f64_m16, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 - svcntd () * 16, z0),
	    svst2 (p0, x0 - svcntd () * 16, z0))

/*
** st2_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_f64_m18, svfloat64x2_t, float64_t,
	    svst2_f64 (p0, x0 - svcntd () * 18, z0),
	    svst2 (p0, x0 - svcntd () * 18, z0))

/*
** st2_vnum_f64_0:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_f64_0, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, 0, z0),
	    svst2_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_f64_1:
**	incb	x0
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_f64_1, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, 1, z0),
	    svst2_vnum (p0, x0, 1, z0))

/*
** st2_vnum_f64_2:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_f64_2, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, 2, z0),
	    svst2_vnum (p0, x0, 2, z0))

/*
** st2_vnum_f64_14:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_f64_14, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, 14, z0),
	    svst2_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_f64_16:
**	incb	x0, all, mul #16
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_f64_16, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, 16, z0),
	    svst2_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_f64_m1:
**	decb	x0
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_f64_m1, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, -1, z0),
	    svst2_vnum (p0, x0, -1, z0))

/*
** st2_vnum_f64_m2:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_f64_m2, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, -2, z0),
	    svst2_vnum (p0, x0, -2, z0))

/*
** st2_vnum_f64_m16:
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_f64_m16, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, -16, z0),
	    svst2_vnum (p0, x0, -16, z0))

/*
** st2_vnum_f64_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_vnum_f64_m18, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, -18, z0),
	    svst2_vnum (p0, x0, -18, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st2_vnum_f64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st2d	{z0\.d(?: - |, )z1\.d}, p0, \[\2\]
**	ret
*/
TEST_STORE (st2_vnum_f64_x1, svfloat64x2_t, float64_t,
	    svst2_vnum_f64 (p0, x0, x1, z0),
	    svst2_vnum (p0, x0, x1, z0))
