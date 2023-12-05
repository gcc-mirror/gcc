/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** st1_u8_base:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_base, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_u8_index:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, x1\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_index, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_1:
**	incb	x0
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_1, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb (), z0),
		  svst1 (pn8, x0 + svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_2:
**	incb	x0, all, mul #2
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_2, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb () * 2, z0),
		  svst1 (pn8, x0 + svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_3:
**	incb	x0, all, mul #3
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_3, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb () * 3, z0),
		  svst1 (pn8, x0 + svcntb () * 3, z0))

/*
** st1_u8_4:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_4, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb () * 4, z0),
		  svst1 (pn8, x0 + svcntb () * 4, z0))

/*
** st1_u8_28:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_28, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb () * 28, z0),
		  svst1 (pn8, x0 + svcntb () * 28, z0))

/*
** st1_u8_32:
**	[^{]*
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_32, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 + svcntb () * 32, z0),
		  svst1 (pn8, x0 + svcntb () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_m1:
**	decb	x0
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m1, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb (), z0),
		  svst1 (pn8, x0 - svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_m2:
**	decb	x0, all, mul #2
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m2, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb () * 2, z0),
		  svst1 (pn8, x0 - svcntb () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u8_m3:
**	decb	x0, all, mul #3
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m3, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb () * 3, z0),
		  svst1 (pn8, x0 - svcntb () * 3, z0))

/*
** st1_u8_m4:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m4, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb () * 4, z0),
		  svst1 (pn8, x0 - svcntb () * 4, z0))

/*
** st1_u8_m32:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m32, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb () * 32, z0),
		  svst1 (pn8, x0 - svcntb () * 32, z0))

/*
** st1_u8_m36:
**	[^{]*
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_m36, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0 - svcntb () * 36, z0),
		  svst1 (pn8, x0 - svcntb () * 36, z0))

/*
** st1_u8_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1b	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_z17, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_u8_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	st1b	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_z22, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_u8_z28:
**	st1b	{z28\.b - z31\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_z28, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_u8_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1b	{z0\.b - z3\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_pn0, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_u8_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1b	{z0\.b - z3\.b}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_pn7, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_u8_pn15:
**	st1b	{z0\.b - z3\.b}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_u8_pn15, svuint8x4_t, uint8_t,
		  svst1_u8_x4 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_u8_0:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_0, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_1:
**	incb	x0
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_1, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_2:
**	incb	x0, all, mul #2
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_2, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_3:
**	incb	x0, all, mul #3
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_3, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 3, z0),
		  svst1_vnum (pn8, x0, 3, z0))

/*
** st1_vnum_u8_4:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_4, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 4, z0),
		  svst1_vnum (pn8, x0, 4, z0))

/*
** st1_vnum_u8_28:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_28, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 28, z0),
		  svst1_vnum (pn8, x0, 28, z0))

/*
** st1_vnum_u8_32:
**	[^{]*
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_32, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, 32, z0),
		  svst1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_m1:
**	decb	x0
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m1, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_m2:
**	decb	x0, all, mul #2
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m2, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u8_m3:
**	decb	x0, all, mul #3
**	st1b	{z0\.b - z3\.b}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m3, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -3, z0),
		  svst1_vnum (pn8, x0, -3, z0))

/*
** st1_vnum_u8_m4:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m4, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -4, z0),
		  svst1_vnum (pn8, x0, -4, z0))

/*
** st1_vnum_u8_m32:
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m32, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -32, z0),
		  svst1_vnum (pn8, x0, -32, z0))

/*
** st1_vnum_u8_m36:
**	[^{]*
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_m36, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, -36, z0),
		  svst1_vnum (pn8, x0, -36, z0))

/*
** st1_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1b	{z0\.b - z3\.b}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1b	{z0\.b - z3\.b}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (st1_vnum_u8_x1, svuint8x4_t, uint8_t,
		  svst1_vnum_u8_x4 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
