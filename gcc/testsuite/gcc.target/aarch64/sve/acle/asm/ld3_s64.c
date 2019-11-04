/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld3_s64_base:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_s64_base, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0),
	   z0 = svld3 (p0, x0))

/*
** ld3_s64_index:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld3_s64_index, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + x1),
	   z0 = svld3 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_s64_1:
**	incb	x0
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_s64_1, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + svcntd ()),
	   z0 = svld3 (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_s64_2:
**	incb	x0, all, mul #2
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_s64_2, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + svcntd () * 2),
	   z0 = svld3 (p0, x0 + svcntd () * 2))

/*
** ld3_s64_3:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_s64_3, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + svcntd () * 3),
	   z0 = svld3 (p0, x0 + svcntd () * 3))

/*
** ld3_s64_21:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #21, mul vl\]
**	ret
*/
TEST_LOAD (ld3_s64_21, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + svcntd () * 21),
	   z0 = svld3 (p0, x0 + svcntd () * 21))

/*
** ld3_s64_24:
**	addvl	(x[0-9]+), x0, #24
**	ld3d	{z0\.d - z2\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_s64_24, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 + svcntd () * 24),
	   z0 = svld3 (p0, x0 + svcntd () * 24))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_s64_m1:
**	decb	x0
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_s64_m1, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 - svcntd ()),
	   z0 = svld3 (p0, x0 - svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_s64_m2:
**	decb	x0, all, mul #2
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_s64_m2, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 - svcntd () * 2),
	   z0 = svld3 (p0, x0 - svcntd () * 2))

/*
** ld3_s64_m3:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #-3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_s64_m3, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 - svcntd () * 3),
	   z0 = svld3 (p0, x0 - svcntd () * 3))

/*
** ld3_s64_m24:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #-24, mul vl\]
**	ret
*/
TEST_LOAD (ld3_s64_m24, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 - svcntd () * 24),
	   z0 = svld3 (p0, x0 - svcntd () * 24))

/*
** ld3_s64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	ld3d	{z0\.d - z2\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_s64_m27, svint64x3_t, int64_t,
	   z0 = svld3_s64 (p0, x0 - svcntd () * 27),
	   z0 = svld3 (p0, x0 - svcntd () * 27))

/*
** ld3_vnum_s64_0:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_0, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 0),
	   z0 = svld3_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_s64_1:
**	incb	x0
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_1, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 1),
	   z0 = svld3_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_s64_2:
**	incb	x0, all, mul #2
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_2, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 2),
	   z0 = svld3_vnum (p0, x0, 2))

/*
** ld3_vnum_s64_3:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_3, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 3),
	   z0 = svld3_vnum (p0, x0, 3))

/*
** ld3_vnum_s64_21:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #21, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_21, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 21),
	   z0 = svld3_vnum (p0, x0, 21))

/*
** ld3_vnum_s64_24:
**	addvl	(x[0-9]+), x0, #24
**	ld3d	{z0\.d - z2\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_24, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, 24),
	   z0 = svld3_vnum (p0, x0, 24))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_s64_m1:
**	decb	x0
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_m1, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, -1),
	   z0 = svld3_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld3_vnum_s64_m2:
**	decb	x0, all, mul #2
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_m2, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, -2),
	   z0 = svld3_vnum (p0, x0, -2))

/*
** ld3_vnum_s64_m3:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #-3, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_m3, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, -3),
	   z0 = svld3_vnum (p0, x0, -3))

/*
** ld3_vnum_s64_m24:
**	ld3d	{z0\.d - z2\.d}, p0/z, \[x0, #-24, mul vl\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_m24, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, -24),
	   z0 = svld3_vnum (p0, x0, -24))

/*
** ld3_vnum_s64_m27:
**	addvl	(x[0-9]+), x0, #-27
**	ld3d	{z0\.d - z2\.d}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_m27, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, -27),
	   z0 = svld3_vnum (p0, x0, -27))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld3_vnum_s64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld3d	{z0\.d - z2\.d}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld3_vnum_s64_x1, svint64x3_t, int64_t,
	   z0 = svld3_vnum_s64 (p0, x0, x1),
	   z0 = svld3_vnum (p0, x0, x1))
