/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** ld1_u8_base:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_base, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0),
		 z0 = svld1_x2 (pn8, x0))

/*
** ld1_u8_index:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, x1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_index, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 + x1),
		 z0 = svld1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_1:
**	incb	x0
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_1, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 + svcntb ()),
		 z0 = svld1_x2 (pn8, x0 + svcntb ()))

/*
** ld1_u8_2:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_2, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 + svcntb () * 2),
		 z0 = svld1_x2 (pn8, x0 + svcntb () * 2))

/*
** ld1_u8_14:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_14, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 + svcntb () * 14),
		 z0 = svld1_x2 (pn8, x0 + svcntb () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_16:
**	incb	x0, all, mul #16
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_16, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 + svcntb () * 16),
		 z0 = svld1_x2 (pn8, x0 + svcntb () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_m1:
**	decb	x0
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m1, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 - svcntb ()),
		 z0 = svld1_x2 (pn8, x0 - svcntb ()))

/*
** ld1_u8_m2:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m2, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 - svcntb () * 2),
		 z0 = svld1_x2 (pn8, x0 - svcntb () * 2))

/*
** ld1_u8_m16:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m16, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 - svcntb () * 16),
		 z0 = svld1_x2 (pn8, x0 - svcntb () * 16))

/*
** ld1_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_m18, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn8, x0 - svcntb () * 18),
		 z0 = svld1_x2 (pn8, x0 - svcntb () * 18))

/*
** ld1_u8_z17:
**	ld1b	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z17, svuint8x2_t, uint8_t,
		 z17 = svld1_u8_x2 (pn8, x0),
		 z17 = svld1_x2 (pn8, x0))

/*
** ld1_u8_z22:
**	ld1b	{z22\.b(?: - |, )z23\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z22, svuint8x2_t, uint8_t,
		 z22 = svld1_u8_x2 (pn8, x0),
		 z22 = svld1_x2 (pn8, x0))

/*
** ld1_u8_z28:
**	ld1b	{z28\.b(?: - |, )z29\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_z28, svuint8x2_t, uint8_t,
		 z28 = svld1_u8_x2 (pn8, x0),
		 z28 = svld1_x2 (pn8, x0))

/*
** ld1_u8_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn0, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn0, x0),
		 z0 = svld1_x2 (pn0, x0))

/*
** ld1_u8_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn7, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn7, x0),
		 z0 = svld1_x2 (pn7, x0))

/*
** ld1_u8_pn15:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_u8_pn15, svuint8x2_t, uint8_t,
		 z0 = svld1_u8_x2 (pn15, x0),
		 z0 = svld1_x2 (pn15, x0))

/*
** ld1_vnum_u8_0:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_0, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, 0),
		 z0 = svld1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_1:
**	incb	x0
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_1, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, 1),
		 z0 = svld1_vnum_x2 (pn8, x0, 1))

/*
** ld1_vnum_u8_2:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_2, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, 2),
		 z0 = svld1_vnum_x2 (pn8, x0, 2))

/*
** ld1_vnum_u8_14:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_14, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, 14),
		 z0 = svld1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_16:
**	incb	x0, all, mul #16
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_16, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, 16),
		 z0 = svld1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_m1:
**	decb	x0
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m1, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, -1),
		 z0 = svld1_vnum_x2 (pn8, x0, -1))

/*
** ld1_vnum_u8_m2:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m2, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, -2),
		 z0 = svld1_vnum_x2 (pn8, x0, -2))

/*
** ld1_vnum_u8_m16:
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m16, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, -16),
		 z0 = svld1_vnum_x2 (pn8, x0, -16))

/*
** ld1_vnum_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_m18, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, -18),
		 z0 = svld1_vnum_x2 (pn8, x0, -18))

/*
** ld1_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1b	{z0\.b(?: - |, )z1\.b}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ld1_vnum_u8_x1, svuint8x2_t, uint8_t,
		 z0 = svld1_vnum_u8_x2 (pn8, x0, x1),
		 z0 = svld1_vnum_x2 (pn8, x0, x1))
