/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_s16_base:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_base, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_s16_index:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_index, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_1:
**	incb	x0
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_1, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth (), z0),
		  svstnt1 (pn8, x0 + svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_2:
**	incb	x0, all, mul #2
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_2, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth () * 2, z0),
		  svstnt1 (pn8, x0 + svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_3:
**	incb	x0, all, mul #3
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_3, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth () * 3, z0),
		  svstnt1 (pn8, x0 + svcnth () * 3, z0))

/*
** stnt1_s16_4:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_4, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth () * 4, z0),
		  svstnt1 (pn8, x0 + svcnth () * 4, z0))

/*
** stnt1_s16_28:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_28, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth () * 28, z0),
		  svstnt1 (pn8, x0 + svcnth () * 28, z0))

/*
** stnt1_s16_32:
**	[^{]*
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_32, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 + svcnth () * 32, z0),
		  svstnt1 (pn8, x0 + svcnth () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_m1:
**	decb	x0
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m1, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth (), z0),
		  svstnt1 (pn8, x0 - svcnth (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_m2:
**	decb	x0, all, mul #2
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m2, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth () * 2, z0),
		  svstnt1 (pn8, x0 - svcnth () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_s16_m3:
**	decb	x0, all, mul #3
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m3, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth () * 3, z0),
		  svstnt1 (pn8, x0 - svcnth () * 3, z0))

/*
** stnt1_s16_m4:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m4, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth () * 4, z0),
		  svstnt1 (pn8, x0 - svcnth () * 4, z0))

/*
** stnt1_s16_m32:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m32, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth () * 32, z0),
		  svstnt1 (pn8, x0 - svcnth () * 32, z0))

/*
** stnt1_s16_m36:
**	[^{]*
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_m36, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0 - svcnth () * 36, z0),
		  svstnt1 (pn8, x0 - svcnth () * 36, z0))

/*
** stnt1_s16_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_z17, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_s16_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_z22, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_s16_z28:
**	stnt1h	{z28\.h - z31\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_z28, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_s16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1h	{z0\.h - z3\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_pn0, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_s16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1h	{z0\.h - z3\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_pn7, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_s16_pn15:
**	stnt1h	{z0\.h - z3\.h}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_s16_pn15, svint16x4_t, int16_t,
		  svstnt1_s16_x4 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_s16_0:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_0, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_1:
**	incb	x0
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_1, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_2:
**	incb	x0, all, mul #2
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_2, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_3:
**	incb	x0, all, mul #3
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_3, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 3, z0),
		  svstnt1_vnum (pn8, x0, 3, z0))

/*
** stnt1_vnum_s16_4:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_4, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 4, z0),
		  svstnt1_vnum (pn8, x0, 4, z0))

/*
** stnt1_vnum_s16_28:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_28, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 28, z0),
		  svstnt1_vnum (pn8, x0, 28, z0))

/*
** stnt1_vnum_s16_32:
**	[^{]*
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_32, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, 32, z0),
		  svstnt1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_m1:
**	decb	x0
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m1, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_m2:
**	decb	x0, all, mul #2
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m2, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_s16_m3:
**	decb	x0, all, mul #3
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m3, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -3, z0),
		  svstnt1_vnum (pn8, x0, -3, z0))

/*
** stnt1_vnum_s16_m4:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m4, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -4, z0),
		  svstnt1_vnum (pn8, x0, -4, z0))

/*
** stnt1_vnum_s16_m32:
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m32, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -32, z0),
		  svstnt1_vnum (pn8, x0, -32, z0))

/*
** stnt1_vnum_s16_m36:
**	[^{]*
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_m36, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, -36, z0),
		  svstnt1_vnum (pn8, x0, -36, z0))

/*
** stnt1_vnum_s16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1h	{z0\.h - z3\.h}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1h	{z0\.h - z3\.h}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_s16_x1, svint16x4_t, int16_t,
		  svstnt1_vnum_s16_x4 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
