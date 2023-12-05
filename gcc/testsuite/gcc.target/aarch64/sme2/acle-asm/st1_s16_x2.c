/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** st1_s16_base:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_base, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_s16_index:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_index, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s16_1:
**	incb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_1, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 + svcnth (), z0),
		  svst1 (pn8, x0 + svcnth (), z0))

/*
** st1_s16_2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_2, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 + svcnth () * 2, z0),
		  svst1 (pn8, x0 + svcnth () * 2, z0))

/*
** st1_s16_14:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_14, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 + svcnth () * 14, z0),
		  svst1 (pn8, x0 + svcnth () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s16_16:
**	incb	x0, all, mul #16
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_16, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 + svcnth () * 16, z0),
		  svst1 (pn8, x0 + svcnth () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_s16_m1:
**	decb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_m1, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 - svcnth (), z0),
		  svst1 (pn8, x0 - svcnth (), z0))

/*
** st1_s16_m2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_m2, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 - svcnth () * 2, z0),
		  svst1 (pn8, x0 - svcnth () * 2, z0))

/*
** st1_s16_m16:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_m16, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 - svcnth () * 16, z0),
		  svst1 (pn8, x0 - svcnth () * 16, z0))

/*
** st1_s16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_m18, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0 - svcnth () * 18, z0),
		  svst1 (pn8, x0 - svcnth () * 18, z0))

/*
** st1_s16_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	st1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_z17, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_s16_z22:
**	st1h	{z22\.h(?: - |, )z23\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_z22, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_s16_z28:
**	st1h	{z28\.h(?: - |, )z29\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_z28, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_s16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1h	{z0\.h(?: - |, )z1\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_pn0, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_s16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1h	{z0\.h(?: - |, )z1\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_pn7, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_s16_pn15:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_s16_pn15, svint16x2_t, int16_t,
		  svst1_s16_x2 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_s16_0:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_0, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s16_1:
**	incb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_1, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/*
** st1_vnum_s16_2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_2, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/*
** st1_vnum_s16_14:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_14, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, 14, z0),
		  svst1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s16_16:
**	incb	x0, all, mul #16
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_16, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, 16, z0),
		  svst1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_s16_m1:
**	decb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_m1, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/*
** st1_vnum_s16_m2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_m2, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/*
** st1_vnum_s16_m16:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_m16, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, -16, z0),
		  svst1_vnum (pn8, x0, -16, z0))

/*
** st1_vnum_s16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_m18, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, -18, z0),
		  svst1_vnum (pn8, x0, -18, z0))

/*
** st1_vnum_s16_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (st1_vnum_s16_x1, svint16x2_t, int16_t,
		  svst1_vnum_s16_x2 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
