/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st3_s64_base:
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s64_base, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0, z0),
	    svst3 (p0, x0, z0))

/*
** st3_s64_index:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, x1, lsl 3\]
**	ret
*/
TEST_STORE (st3_s64_index, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + x1, z0),
	    svst3 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s64_1:
**	incb	x0
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s64_1, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + svcntd (), z0),
	    svst3 (p0, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s64_2:
**	incb	x0, all, mul #2
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s64_2, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + svcntd () * 2, z0),
	    svst3 (p0, x0 + svcntd () * 2, z0))

/*
** st3_s64_3:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_s64_3, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + svcntd () * 3, z0),
	    svst3 (p0, x0 + svcntd () * 3, z0))

/*
** st3_s64_21:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_s64_21, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + svcntd () * 21, z0),
	    svst3 (p0, x0 + svcntd () * 21, z0))

/*
** st3_s64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3d	{z0\.d - z2\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_s64_24, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 + svcntd () * 24, z0),
	    svst3 (p0, x0 + svcntd () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s64_m1:
**	decb	x0
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s64_m1, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 - svcntd (), z0),
	    svst3 (p0, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s64_m2:
**	decb	x0, all, mul #2
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s64_m2, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 - svcntd () * 2, z0),
	    svst3 (p0, x0 - svcntd () * 2, z0))

/*
** st3_s64_m3:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_s64_m3, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 - svcntd () * 3, z0),
	    svst3 (p0, x0 - svcntd () * 3, z0))

/*
** st3_s64_m24:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_s64_m24, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 - svcntd () * 24, z0),
	    svst3 (p0, x0 - svcntd () * 24, z0))

/*
** st3_s64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3d	{z0\.d - z2\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_s64_m27, svint64x3_t, int64_t,
	    svst3_s64 (p0, x0 - svcntd () * 27, z0),
	    svst3 (p0, x0 - svcntd () * 27, z0))

/*
** st3_vnum_s64_0:
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s64_0, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 0, z0),
	    svst3_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s64_1:
**	incb	x0
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s64_1, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 1, z0),
	    svst3_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s64_2:
**	incb	x0, all, mul #2
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s64_2, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 2, z0),
	    svst3_vnum (p0, x0, 2, z0))

/*
** st3_vnum_s64_3:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s64_3, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 3, z0),
	    svst3_vnum (p0, x0, 3, z0))

/*
** st3_vnum_s64_21:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s64_21, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 21, z0),
	    svst3_vnum (p0, x0, 21, z0))

/*
** st3_vnum_s64_24:
**	addvl	(x[0-9]+), x0, #24
**	st3d	{z0\.d - z2\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_s64_24, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, 24, z0),
	    svst3_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s64_m1:
**	decb	x0
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s64_m1, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, -1, z0),
	    svst3_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s64_m2:
**	decb	x0, all, mul #2
**	st3d	{z0\.d - z2\.d}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s64_m2, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, -2, z0),
	    svst3_vnum (p0, x0, -2, z0))

/*
** st3_vnum_s64_m3:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s64_m3, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, -3, z0),
	    svst3_vnum (p0, x0, -3, z0))

/*
** st3_vnum_s64_m24:
**	st3d	{z0\.d - z2\.d}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s64_m24, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, -24, z0),
	    svst3_vnum (p0, x0, -24, z0))

/*
** st3_vnum_s64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3d	{z0\.d - z2\.d}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_s64_m27, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, -27, z0),
	    svst3_vnum (p0, x0, -27, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st3_vnum_s64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st3d	{z0\.d - z2\.d}, p0, \[\2\]
**	ret
*/
TEST_STORE (st3_vnum_s64_x1, svint64x3_t, int64_t,
	    svst3_vnum_s64 (p0, x0, x1, z0),
	    svst3_vnum (p0, x0, x1, z0))
