/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ldnt1_s16_base:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_base, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0),
		 z0 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_s16_index:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_index, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + x1),
		 z0 = svldnt1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_1:
**	incb	x0
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_1, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth ()),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_2:
**	incb	x0, all, mul #2
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_2, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth () * 2),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_3:
**	incb	x0, all, mul #3
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_3, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth () * 3),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth () * 3))

/*
** ldnt1_s16_4:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_4, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth () * 4),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth () * 4))

/*
** ldnt1_s16_28:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_28, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth () * 28),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth () * 28))

/*
** ldnt1_s16_32:
**	[^{]*
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_32, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 + svcnth () * 32),
		 z0 = svldnt1_x4 (pn8, x0 + svcnth () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_m1:
**	decb	x0
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_m1, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 - svcnth ()),
		 z0 = svldnt1_x4 (pn8, x0 - svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_m2:
**	decb	x0, all, mul #2
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_m2, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 - svcnth () * 2),
		 z0 = svldnt1_x4 (pn8, x0 - svcnth () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s16_m3:
**	decb	x0, all, mul #3
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_m3, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 - svcnth () * 3),
		 z0 = svldnt1_x4 (pn8, x0 - svcnth () * 3))

/*
** ldnt1_s16_m4:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ldnt1_s16_m4, svint16x4_t, int16_t,
		   z0 = svldnt1_s16_x4 (pn8, x0 - svcnth () * 4),
		   z0 = svldnt1_x4 (pn8, x0 - svcnth () * 4))

/*
** ldnt1_s16_m32:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_m32, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 - svcnth () * 32),
		 z0 = svldnt1_x4 (pn8, x0 - svcnth () * 32))

/*
** ldnt1_s16_m36:
**	[^{]*
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_m36, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn8, x0 - svcnth () * 36),
		 z0 = svldnt1_x4 (pn8, x0 - svcnth () * 36))

/*
** ldnt1_s16_z17:
**	ldnt1h	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_z17, svint16x4_t, int16_t,
		 z17 = svldnt1_s16_x4 (pn8, x0),
		 z17 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_s16_z22:
**	ldnt1h	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_z22, svint16x4_t, int16_t,
		 z22 = svldnt1_s16_x4 (pn8, x0),
		 z22 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_s16_z28:
**	ldnt1h	{z28\.h(?: - |, )z31\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_z28, svint16x4_t, int16_t,
		 z28 = svldnt1_s16_x4 (pn8, x0),
		 z28 = svldnt1_x4 (pn8, x0))

/*
** ldnt1_s16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1h	{z0\.h(?: - |, )z3\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_pn0, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn0, x0),
		 z0 = svldnt1_x4 (pn0, x0))

/*
** ldnt1_s16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1h	{z0\.h(?: - |, )z3\.h}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_pn7, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn7, x0),
		 z0 = svldnt1_x4 (pn7, x0))

/*
** ldnt1_s16_pn15:
**	ldnt1h	{z0\.h(?: - |, )z3\.h}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_s16_pn15, svint16x4_t, int16_t,
		 z0 = svldnt1_s16_x4 (pn15, x0),
		 z0 = svldnt1_x4 (pn15, x0))

/*
** ldnt1_vnum_s16_0:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_0, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_1:
**	incb	x0
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_1, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_2:
**	incb	x0, all, mul #2
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_2, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_3:
**	incb	x0, all, mul #3
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_3, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 3),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 3))

/*
** ldnt1_vnum_s16_4:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_4, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 4),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 4))

/*
** ldnt1_vnum_s16_28:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_28, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 28),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 28))

/*
** ldnt1_vnum_s16_32:
**	[^{]*
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_32, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, 32),
		 z0 = svldnt1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_m1:
**	decb	x0
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m1, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_m2:
**	decb	x0, all, mul #2
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m2, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s16_m3:
**	decb	x0, all, mul #3
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m3, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -3),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -3))

/*
** ldnt1_vnum_s16_m4:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m4, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -4),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -4))

/*
** ldnt1_vnum_s16_m32:
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m32, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -32),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -32))

/*
** ldnt1_vnum_s16_m36:
**	[^{]*
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_m36, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, -36),
		 z0 = svldnt1_vnum_x4 (pn8, x0, -36))

/*
** ldnt1_vnum_s16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1h	{z0\.h - z3\.h}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_s16_x1, svint16x4_t, int16_t,
		 z0 = svldnt1_vnum_s16_x4 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x4 (pn8, x0, x1))
