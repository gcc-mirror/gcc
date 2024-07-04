/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ld1_u8_base:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_base, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0),
		 z0 = svld1_x4 (pn8, x0))

/*
** ld1_u8_index:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, x1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_index, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + x1),
		 z0 = svld1_x4 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_1:
**	incb	x0
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_1, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb ()),
		 z0 = svld1_x4 (pn8, x0 + svcntb ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_2:
**	incb	x0, all, mul #2
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_2, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb () * 2),
		 z0 = svld1_x4 (pn8, x0 + svcntb () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_3:
**	incb	x0, all, mul #3
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_3, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb () * 3),
		 z0 = svld1_x4 (pn8, x0 + svcntb () * 3))

/*
** ld1_u8_4:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_4, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb () * 4),
		 z0 = svld1_x4 (pn8, x0 + svcntb () * 4))

/*
** ld1_u8_28:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_28, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb () * 28),
		 z0 = svld1_x4 (pn8, x0 + svcntb () * 28))

/*
** ld1_u8_32:
**	[^{]*
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_32, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 + svcntb () * 32),
		 z0 = svld1_x4 (pn8, x0 + svcntb () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_m1:
**	decb	x0
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m1, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 - svcntb ()),
		 z0 = svld1_x4 (pn8, x0 - svcntb ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_m2:
**	decb	x0, all, mul #2
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m2, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 - svcntb () * 2),
		 z0 = svld1_x4 (pn8, x0 - svcntb () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_m3:
**	decb	x0, all, mul #3
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m3, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 - svcntb () * 3),
		 z0 = svld1_x4 (pn8, x0 - svcntb () * 3))

/*
** ld1_u8_m4:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
  TEST_LOAD_COUNT (ld1_u8_m4, svuint8x4_t, uint8_t,
		   z0 = svld1_u8_x4 (pn8, x0 - svcntb () * 4),
		   z0 = svld1_x4 (pn8, x0 - svcntb () * 4))

/*
** ld1_u8_m32:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m32, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 - svcntb () * 32),
		 z0 = svld1_x4 (pn8, x0 - svcntb () * 32))

/*
** ld1_u8_m36:
**	[^{]*
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m36, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn8, x0 - svcntb () * 36),
		 z0 = svld1_x4 (pn8, x0 - svcntb () * 36))

/*
** ld1_u8_z17:
**	ld1b	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z17, svuint8x4_t, uint8_t,
		 z17 = svld1_u8_x4 (pn8, x0),
		 z17 = svld1_x4 (pn8, x0))

/*
** ld1_u8_z22:
**	ld1b	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z22, svuint8x4_t, uint8_t,
		 z22 = svld1_u8_x4 (pn8, x0),
		 z22 = svld1_x4 (pn8, x0))

/*
** ld1_u8_z28:
**	ld1b	{z28\.b(?: - |, )z31\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z28, svuint8x4_t, uint8_t,
		 z28 = svld1_u8_x4 (pn8, x0),
		 z28 = svld1_x4 (pn8, x0))

/*
** ld1_u8_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1b	{z0\.b(?: - |, )z3\.b}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn0, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn0, x0),
		 z0 = svld1_x4 (pn0, x0))

/*
** ld1_u8_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1b	{z0\.b(?: - |, )z3\.b}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn7, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn7, x0),
		 z0 = svld1_x4 (pn7, x0))

/*
** ld1_u8_pn15:
**	ld1b	{z0\.b(?: - |, )z3\.b}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn15, svuint8x4_t, uint8_t,
		 z0 = svld1_u8_x4 (pn15, x0),
		 z0 = svld1_x4 (pn15, x0))

/*
** ld1_vnum_u8_0:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_0, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 0),
		 z0 = svld1_vnum_x4 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_1:
**	incb	x0
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_1, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 1),
		 z0 = svld1_vnum_x4 (pn8, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_2:
**	incb	x0, all, mul #2
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_2, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 2),
		 z0 = svld1_vnum_x4 (pn8, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_3:
**	incb	x0, all, mul #3
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_3, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 3),
		 z0 = svld1_vnum_x4 (pn8, x0, 3))

/*
** ld1_vnum_u8_4:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_4, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 4),
		 z0 = svld1_vnum_x4 (pn8, x0, 4))

/*
** ld1_vnum_u8_28:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_28, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 28),
		 z0 = svld1_vnum_x4 (pn8, x0, 28))

/*
** ld1_vnum_u8_32:
**	[^{]*
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_32, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, 32),
		 z0 = svld1_vnum_x4 (pn8, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_m1:
**	decb	x0
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m1, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -1),
		 z0 = svld1_vnum_x4 (pn8, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_m2:
**	decb	x0, all, mul #2
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m2, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -2),
		 z0 = svld1_vnum_x4 (pn8, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_m3:
**	decb	x0, all, mul #3
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m3, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -3),
		 z0 = svld1_vnum_x4 (pn8, x0, -3))

/*
** ld1_vnum_u8_m4:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m4, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -4),
		 z0 = svld1_vnum_x4 (pn8, x0, -4))

/*
** ld1_vnum_u8_m32:
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m32, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -32),
		 z0 = svld1_vnum_x4 (pn8, x0, -32))

/*
** ld1_vnum_u8_m36:
**	[^{]*
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, x[0-9]+\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m36, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, -36),
		 z0 = svld1_vnum_x4 (pn8, x0, -36))

/*
** ld1_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1b	{z0\.b - z3\.b}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_x1, svuint8x4_t, uint8_t,
		 z0 = svld1_vnum_u8_x4 (pn8, x0, x1),
		 z0 = svld1_vnum_x4 (pn8, x0, x1))
