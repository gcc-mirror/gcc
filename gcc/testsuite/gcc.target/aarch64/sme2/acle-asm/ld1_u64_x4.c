/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ld1_u64_base:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_base, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0),
		 z0 = svld1_x4 (pn8, x0))

/*
** ld1_u64_index:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_index, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + x1),
		 z0 = svld1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_1:
**	incb	x0
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_1, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd ()),
		 z0 = svld1_x4 (pn8, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_2:
**	incb	x0, all, mul #2
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_2, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd () * 2),
		 z0 = svld1_x4 (pn8, x0 + svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_3:
**	incb	x0, all, mul #3
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_3, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd () * 3),
		 z0 = svld1_x4 (pn8, x0 + svcntd () * 3))

/*
** ld1_u64_4:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_4, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd () * 4),
		 z0 = svld1_x4 (pn8, x0 + svcntd () * 4))

/*
** ld1_u64_28:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_28, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd () * 28),
		 z0 = svld1_x4 (pn8, x0 + svcntd () * 28))

/*
** ld1_u64_32:
**	[^{]*
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_32, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 + svcntd () * 32),
		 z0 = svld1_x4 (pn8, x0 + svcntd () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_m1:
**	decb	x0
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_m1, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 - svcntd ()),
		 z0 = svld1_x4 (pn8, x0 - svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_m2:
**	decb	x0, all, mul #2
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_m2, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 - svcntd () * 2),
		 z0 = svld1_x4 (pn8, x0 - svcntd () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u64_m3:
**	decb	x0, all, mul #3
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_m3, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 - svcntd () * 3),
		 z0 = svld1_x4 (pn8, x0 - svcntd () * 3))

/*
** ld1_u64_m4:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ld1_u64_m4, svuint64x4_t, uint64_t,
		   z0 = svld1_u64_x4 (pn8, x0 - svcntd () * 4),
		   z0 = svld1_x4 (pn8, x0 - svcntd () * 4))

/*
** ld1_u64_m32:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_m32, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 - svcntd () * 32),
		 z0 = svld1_x4 (pn8, x0 - svcntd () * 32))

/*
** ld1_u64_m36:
**	[^{]*
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_m36, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn8, x0 - svcntd () * 36),
		 z0 = svld1_x4 (pn8, x0 - svcntd () * 36))

/*
** ld1_u64_z17:
**	ld1d	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_z17, svuint64x4_t, uint64_t,
		 z17 = svld1_u64_x4 (pn8, x0),
		 z17 = svld1_x4 (pn8, x0))

/*
** ld1_u64_z22:
**	ld1d	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_z22, svuint64x4_t, uint64_t,
		 z22 = svld1_u64_x4 (pn8, x0),
		 z22 = svld1_x4 (pn8, x0))

/*
** ld1_u64_z28:
**	ld1d	{z28\.d(?: - |, )z31\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_z28, svuint64x4_t, uint64_t,
		 z28 = svld1_u64_x4 (pn8, x0),
		 z28 = svld1_x4 (pn8, x0))

/*
** ld1_u64_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1d	{z0\.d(?: - |, )z3\.d}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_pn0, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn0, x0),
		 z0 = svld1_x4 (pn0, x0))

/*
** ld1_u64_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1d	{z0\.d(?: - |, )z3\.d}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_pn7, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn7, x0),
		 z0 = svld1_x4 (pn7, x0))

/*
** ld1_u64_pn15:
**	ld1d	{z0\.d(?: - |, )z3\.d}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u64_pn15, svuint64x4_t, uint64_t,
		 z0 = svld1_u64_x4 (pn15, x0),
		 z0 = svld1_x4 (pn15, x0))

/*
** ld1_vnum_u64_0:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_0, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 0),
		 z0 = svld1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_1:
**	incb	x0
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_1, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 1),
		 z0 = svld1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_2:
**	incb	x0, all, mul #2
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_2, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 2),
		 z0 = svld1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_3:
**	incb	x0, all, mul #3
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_3, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 3),
		 z0 = svld1_vnum_x4 (pn8, x0, 3))

/*
** ld1_vnum_u64_4:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_4, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 4),
		 z0 = svld1_vnum_x4 (pn8, x0, 4))

/*
** ld1_vnum_u64_28:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_28, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 28),
		 z0 = svld1_vnum_x4 (pn8, x0, 28))

/*
** ld1_vnum_u64_32:
**	[^{]*
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_32, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, 32),
		 z0 = svld1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_m1:
**	decb	x0
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m1, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -1),
		 z0 = svld1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_m2:
**	decb	x0, all, mul #2
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m2, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -2),
		 z0 = svld1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u64_m3:
**	decb	x0, all, mul #3
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m3, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -3),
		 z0 = svld1_vnum_x4 (pn8, x0, -3))

/*
** ld1_vnum_u64_m4:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m4, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -4),
		 z0 = svld1_vnum_x4 (pn8, x0, -4))

/*
** ld1_vnum_u64_m32:
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m32, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -32),
		 z0 = svld1_vnum_x4 (pn8, x0, -32))

/*
** ld1_vnum_u64_m36:
**	[^{]*
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_m36, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, -36),
		 z0 = svld1_vnum_x4 (pn8, x0, -36))

/*
** ld1_vnum_u64_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1d	{z0\.d - z3\.d}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u64_x1, svuint64x4_t, uint64_t,
		 z0 = svld1_vnum_u64_x4 (pn8, x0, x1),
		 z0 = svld1_vnum_x4 (pn8, x0, x1))
