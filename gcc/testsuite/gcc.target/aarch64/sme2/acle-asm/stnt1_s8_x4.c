/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_s8_base:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_base, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_s8_index:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, x1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_index, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_1:
**	incb	x0
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_1, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb (), z0),
		  svstnt1 (pn8, x0 + svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_2:
**	incb	x0, all, mul #2
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_2, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb () * 2, z0),
		  svstnt1 (pn8, x0 + svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_3:
**	incb	x0, all, mul #3
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_3, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb () * 3, z0),
		  svstnt1 (pn8, x0 + svcntb () * 3, z0))

/*
** stnt1_s8_4:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_4, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb () * 4, z0),
		  svstnt1 (pn8, x0 + svcntb () * 4, z0))

/*
** stnt1_s8_28:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_28, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb () * 28, z0),
		  svstnt1 (pn8, x0 + svcntb () * 28, z0))

/*
** stnt1_s8_32:
**	[^{]*
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_32, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 + svcntb () * 32, z0),
		  svstnt1 (pn8, x0 + svcntb () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_m1:
**	decb	x0
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m1, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb (), z0),
		  svstnt1 (pn8, x0 - svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_m2:
**	decb	x0, all, mul #2
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m2, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb () * 2, z0),
		  svstnt1 (pn8, x0 - svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s8_m3:
**	decb	x0, all, mul #3
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m3, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb () * 3, z0),
		  svstnt1 (pn8, x0 - svcntb () * 3, z0))

/*
** stnt1_s8_m4:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m4, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb () * 4, z0),
		  svstnt1 (pn8, x0 - svcntb () * 4, z0))

/*
** stnt1_s8_m32:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m32, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb () * 32, z0),
		  svstnt1 (pn8, x0 - svcntb () * 32, z0))

/*
** stnt1_s8_m36:
**	[^{]*
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_m36, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0 - svcntb () * 36, z0),
		  svstnt1 (pn8, x0 - svcntb () * 36, z0))

/*
** stnt1_s8_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1b	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_z17, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_s8_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1b	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_z22, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_s8_z28:
**	stnt1b	{z28\.b - z31\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_z28, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_s8_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1b	{z0\.b - z3\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_pn0, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_s8_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1b	{z0\.b - z3\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_pn7, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_s8_pn15:
**	stnt1b	{z0\.b - z3\.b}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s8_pn15, svint8x4_t, int8_t,
		  svstnt1_s8_x4 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_s8_0:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_0, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_1:
**	incb	x0
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_1, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_2:
**	incb	x0, all, mul #2
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_2, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_3:
**	incb	x0, all, mul #3
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_3, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 3, z0),
		  svstnt1_vnum (pn8, x0, 3, z0))

/*
** stnt1_vnum_s8_4:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_4, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 4, z0),
		  svstnt1_vnum (pn8, x0, 4, z0))

/*
** stnt1_vnum_s8_28:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_28, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 28, z0),
		  svstnt1_vnum (pn8, x0, 28, z0))

/*
** stnt1_vnum_s8_32:
**	[^{]*
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_32, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, 32, z0),
		  svstnt1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_m1:
**	decb	x0
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m1, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_m2:
**	decb	x0, all, mul #2
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m2, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s8_m3:
**	decb	x0, all, mul #3
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m3, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -3, z0),
		  svstnt1_vnum (pn8, x0, -3, z0))

/*
** stnt1_vnum_s8_m4:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m4, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -4, z0),
		  svstnt1_vnum (pn8, x0, -4, z0))

/*
** stnt1_vnum_s8_m32:
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m32, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -32, z0),
		  svstnt1_vnum (pn8, x0, -32, z0))

/*
** stnt1_vnum_s8_m36:
**	[^{]*
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_m36, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, -36, z0),
		  svstnt1_vnum (pn8, x0, -36, z0))

/*
** stnt1_vnum_s8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1b	{z0\.b - z3\.b}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1b	{z0\.b - z3\.b}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s8_x1, svint8x4_t, int8_t,
		  svstnt1_vnum_s8_x4 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
