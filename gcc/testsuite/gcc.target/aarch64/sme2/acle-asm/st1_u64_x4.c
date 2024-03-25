/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** st1_u64_base:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_base, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_u64_index:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_index, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_1:
**	incb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_1, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd (), z0),
		  svst1 (pn8, x0 + svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_2:
**	incb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_2, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd () * 2, z0),
		  svst1 (pn8, x0 + svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_3:
**	incb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_3, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd () * 3, z0),
		  svst1 (pn8, x0 + svcntd () * 3, z0))

/*
** st1_u64_4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_4, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd () * 4, z0),
		  svst1 (pn8, x0 + svcntd () * 4, z0))

/*
** st1_u64_28:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_28, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd () * 28, z0),
		  svst1 (pn8, x0 + svcntd () * 28, z0))

/*
** st1_u64_32:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_32, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 + svcntd () * 32, z0),
		  svst1 (pn8, x0 + svcntd () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_m1:
**	decb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m1, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd (), z0),
		  svst1 (pn8, x0 - svcntd (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_m2:
**	decb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m2, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd () * 2, z0),
		  svst1 (pn8, x0 - svcntd () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_m3:
**	decb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m3, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd () * 3, z0),
		  svst1 (pn8, x0 - svcntd () * 3, z0))

/*
** st1_u64_m4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m4, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd () * 4, z0),
		  svst1 (pn8, x0 - svcntd () * 4, z0))

/*
** st1_u64_m32:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m32, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd () * 32, z0),
		  svst1 (pn8, x0 - svcntd () * 32, z0))

/*
** st1_u64_m36:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_m36, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0 - svcntd () * 36, z0),
		  svst1 (pn8, x0 - svcntd () * 36, z0))

/*
** st1_u64_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_z17, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_u64_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1d	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_z22, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_u64_z28:
**	st1d	{z28\.d - z31\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_z28, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_u64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_pn0, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_u64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1d	{z0\.d - z3\.d}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_pn7, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_u64_pn15:
**	st1d	{z0\.d - z3\.d}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u64_pn15, svuint64x4_t, uint64_t,
		  svst1_u64_x4 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_u64_0:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_0, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_1:
**	incb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_1, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_2:
**	incb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_2, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_3:
**	incb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_3, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 3, z0),
		  svst1_vnum (pn8, x0, 3, z0))

/*
** st1_vnum_u64_4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_4, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 4, z0),
		  svst1_vnum (pn8, x0, 4, z0))

/*
** st1_vnum_u64_28:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_28, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 28, z0),
		  svst1_vnum (pn8, x0, 28, z0))

/*
** st1_vnum_u64_32:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_32, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, 32, z0),
		  svst1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_m1:
**	decb	x0
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m1, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_m2:
**	decb	x0, all, mul #2
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m2, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_m3:
**	decb	x0, all, mul #3
**	st1d	{z0\.d - z3\.d}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m3, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -3, z0),
		  svst1_vnum (pn8, x0, -3, z0))

/*
** st1_vnum_u64_m4:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m4, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -4, z0),
		  svst1_vnum (pn8, x0, -4, z0))

/*
** st1_vnum_u64_m32:
**	st1d	{z0\.d - z3\.d}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m32, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -32, z0),
		  svst1_vnum (pn8, x0, -32, z0))

/*
** st1_vnum_u64_m36:
**	[^{]*
**	st1d	{z0\.d - z3\.d}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u64_m36, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, -36, z0),
		  svst1_vnum (pn8, x0, -36, z0))

/*
** st1_vnum_u64_x1:
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
TEST_STORE_COUNT (st1_vnum_u64_x1, svuint64x4_t, uint64_t,
		  svst1_vnum_u64_x4 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
