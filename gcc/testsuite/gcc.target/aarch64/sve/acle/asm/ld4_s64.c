/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld4_s64_base:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_base, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0),
	   z0 = svld4 (p0, x0))

/*
** ld4_s64_index:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld4_s64_index, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + x1),
	   z0 = svld4 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_1:
**	incb	x0
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_1, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd ()),
	   z0 = svld4 (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_2:
**	incb	x0, all, mul #2
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_2, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd () * 2),
	   z0 = svld4 (p0, x0 + svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_3:
**	incb	x0, all, mul #3
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_3, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd () * 3),
	   z0 = svld4 (p0, x0 + svcntd () * 3))

/*
** ld4_s64_4:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s64_4, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd () * 4),
	   z0 = svld4 (p0, x0 + svcntd () * 4))

/*
** ld4_s64_28:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s64_28, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd () * 28),
	   z0 = svld4 (p0, x0 + svcntd () * 28))

/*
** ld4_s64_32:
**	[^{]*
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_s64_32, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 + svcntd () * 32),
	   z0 = svld4 (p0, x0 + svcntd () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_m1:
**	decb	x0
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_m1, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd ()),
	   z0 = svld4 (p0, x0 - svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_m2:
**	decb	x0, all, mul #2
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_m2, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd () * 2),
	   z0 = svld4 (p0, x0 - svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s64_m3:
**	decb	x0, all, mul #3
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s64_m3, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd () * 3),
	   z0 = svld4 (p0, x0 - svcntd () * 3))

/*
** ld4_s64_m4:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s64_m4, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd () * 4),
	   z0 = svld4 (p0, x0 - svcntd () * 4))

/*
** ld4_s64_m32:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s64_m32, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd () * 32),
	   z0 = svld4 (p0, x0 - svcntd () * 32))

/*
** ld4_s64_m36:
**	[^{]*
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_s64_m36, svint64x4_t, int64_t,
	   z0 = svld4_s64 (p0, x0 - svcntd () * 36),
	   z0 = svld4 (p0, x0 - svcntd () * 36))

/*
** ld4_vnum_s64_0:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_0, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 0),
	   z0 = svld4_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_1:
**	incb	x0
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_1, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 1),
	   z0 = svld4_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_2:
**	incb	x0, all, mul #2
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_2, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 2),
	   z0 = svld4_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_3:
**	incb	x0, all, mul #3
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_3, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 3),
	   z0 = svld4_vnum (p0, x0, 3))

/*
** ld4_vnum_s64_4:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_4, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 4),
	   z0 = svld4_vnum (p0, x0, 4))

/*
** ld4_vnum_s64_28:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_28, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 28),
	   z0 = svld4_vnum (p0, x0, 28))

/*
** ld4_vnum_s64_32:
**	[^{]*
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_32, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, 32),
	   z0 = svld4_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_m1:
**	decb	x0
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m1, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -1),
	   z0 = svld4_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_m2:
**	decb	x0, all, mul #2
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m2, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -2),
	   z0 = svld4_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s64_m3:
**	decb	x0, all, mul #3
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m3, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -3),
	   z0 = svld4_vnum (p0, x0, -3))

/*
** ld4_vnum_s64_m4:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m4, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -4),
	   z0 = svld4_vnum (p0, x0, -4))

/*
** ld4_vnum_s64_m32:
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m32, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -32),
	   z0 = svld4_vnum (p0, x0, -32))

/*
** ld4_vnum_s64_m36:
**	[^{]*
**	ld4d	{z0\.d - z3\.d}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_m36, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, -36),
	   z0 = svld4_vnum (p0, x0, -36))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld4_vnum_s64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld4d	{z0\.d - z3\.d}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld4_vnum_s64_x1, svint64x4_t, int64_t,
	   z0 = svld4_vnum_s64 (p0, x0, x1),
	   z0 = svld4_vnum (p0, x0, x1))
