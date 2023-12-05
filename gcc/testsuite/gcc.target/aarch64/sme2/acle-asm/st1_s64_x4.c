/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** st1_s64_base:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_base, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_s64_index:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_index, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_1:
**	incb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_1, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd (), z0),
		  svst1 (pn8, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_2:
**	incb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_2, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd () * 2, z0),
		  svst1 (pn8, x0 + svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_3:
**	incb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_3, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd () * 3, z0),
		  svst1 (pn8, x0 + svcntd () * 3, z0))

/*
** st1_s64_4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_4, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd () * 4, z0),
		  svst1 (pn8, x0 + svcntd () * 4, z0))

/*
** st1_s64_28:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_28, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd () * 28, z0),
		  svst1 (pn8, x0 + svcntd () * 28, z0))

/*
** st1_s64_32:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_32, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 + svcntd () * 32, z0),
		  svst1 (pn8, x0 + svcntd () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_m1:
**	decb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m1, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd (), z0),
		  svst1 (pn8, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_m2:
**	decb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m2, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd () * 2, z0),
		  svst1 (pn8, x0 - svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s64_m3:
**	decb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m3, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd () * 3, z0),
		  svst1 (pn8, x0 - svcntd () * 3, z0))

/*
** st1_s64_m4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m4, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd () * 4, z0),
		  svst1 (pn8, x0 - svcntd () * 4, z0))

/*
** st1_s64_m32:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m32, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd () * 32, z0),
		  svst1 (pn8, x0 - svcntd () * 32, z0))

/*
** st1_s64_m36:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_m36, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0 - svcntd () * 36, z0),
		  svst1 (pn8, x0 - svcntd () * 36, z0))

/*
** st1_s64_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_z17, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_s64_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_z22, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_s64_z28:
**	st1d	{z28\.d - z31\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_z28, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_s64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_pn0, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_s64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_pn7, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_s64_pn15:
**	st1d	{z0\.d - z3\.d}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s64_pn15, svint64x4_t, int64_t,
		  svst1_s64_x4 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_s64_0:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_0, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_1:
**	incb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_1, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_2:
**	incb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_2, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_3:
**	incb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_3, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 3, z0),
		  svst1_vnum (pn8, x0, 3, z0))

/*
** st1_vnum_s64_4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_4, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 4, z0),
		  svst1_vnum (pn8, x0, 4, z0))

/*
** st1_vnum_s64_28:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_28, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 28, z0),
		  svst1_vnum (pn8, x0, 28, z0))

/*
** st1_vnum_s64_32:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_32, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, 32, z0),
		  svst1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_m1:
**	decb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m1, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_m2:
**	decb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m2, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s64_m3:
**	decb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m3, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -3, z0),
		  svst1_vnum (pn8, x0, -3, z0))

/*
** st1_vnum_s64_m4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m4, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -4, z0),
		  svst1_vnum (pn8, x0, -4, z0))

/*
** st1_vnum_s64_m32:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m32, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -32, z0),
		  svst1_vnum (pn8, x0, -32, z0))

/*
** st1_vnum_s64_m36:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_m36, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, -36, z0),
		  svst1_vnum (pn8, x0, -36, z0))

/*
** st1_vnum_s64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1d	{z0\.d - z3\.d}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s64_x1, svint64x4_t, int64_t,
		  svst1_vnum_s64_x4 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
