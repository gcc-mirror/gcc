/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** st1_u16_base:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_base, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_u16_index:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_index, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u16_1:
**	incb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_1, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 + svcnth (), z0),
		  svst1 (pn8, x0 + svcnth (), z0))

/*
** st1_u16_2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_2, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 + svcnth () * 2, z0),
		  svst1 (pn8, x0 + svcnth () * 2, z0))

/*
** st1_u16_14:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_14, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 + svcnth () * 14, z0),
		  svst1 (pn8, x0 + svcnth () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u16_16:
**	incb	x0, all, mul #16
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_16, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 + svcnth () * 16, z0),
		  svst1 (pn8, x0 + svcnth () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u16_m1:
**	decb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_m1, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 - svcnth (), z0),
		  svst1 (pn8, x0 - svcnth (), z0))

/*
** st1_u16_m2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_m2, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 - svcnth () * 2, z0),
		  svst1 (pn8, x0 - svcnth () * 2, z0))

/*
** st1_u16_m16:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_m16, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 - svcnth () * 16, z0),
		  svst1 (pn8, x0 - svcnth () * 16, z0))

/*
** st1_u16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_m18, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0 - svcnth () * 18, z0),
		  svst1 (pn8, x0 - svcnth () * 18, z0))

/*
** st1_u16_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	st1h	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_z17, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_u16_z22:
**	st1h	{z22\.h(?: - |, )z23\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_z22, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_u16_z28:
**	st1h	{z28\.h(?: - |, )z29\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_z28, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_u16_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1h	{z0\.h(?: - |, )z1\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_pn0, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_u16_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1h	{z0\.h(?: - |, )z1\.h}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_pn7, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_u16_pn15:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u16_pn15, svuint16x2_t, uint16_t,
		  svst1_u16_x2 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_u16_0:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_0, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u16_1:
**	incb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_1, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/*
** st1_vnum_u16_2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_2, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/*
** st1_vnum_u16_14:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_14, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, 14, z0),
		  svst1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u16_16:
**	incb	x0, all, mul #16
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_16, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, 16, z0),
		  svst1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u16_m1:
**	decb	x0
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_m1, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/*
** st1_vnum_u16_m2:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_m2, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/*
** st1_vnum_u16_m16:
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_m16, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, -16, z0),
		  svst1_vnum (pn8, x0, -16, z0))

/*
** st1_vnum_u16_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1h	{z0\.h(?: - |, )z1\.h}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u16_m18, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, -18, z0),
		  svst1_vnum (pn8, x0, -18, z0))

/*
** st1_vnum_u16_x1:
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
TEST_STORE_COUNT (st1_vnum_u16_x1, svuint16x2_t, uint16_t,
		  svst1_vnum_u16_x2 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
