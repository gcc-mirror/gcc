/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld2_s8_base:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_s8_base, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0),
	   z0 = svld2 (p0, x0))

/*
** ld2_s8_index:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld2_s8_index, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 + x1),
	   z0 = svld2 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_s8_1:
**	incb	x0
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_s8_1, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 + svcntb ()),
	   z0 = svld2 (p0, x0 + svcntb ()))

/*
** ld2_s8_2:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_s8_2, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 + svcntb () * 2),
	   z0 = svld2 (p0, x0 + svcntb () * 2))

/*
** ld2_s8_14:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2_s8_14, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 + svcntb () * 14),
	   z0 = svld2 (p0, x0 + svcntb () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_s8_16:
**	incb	x0, all, mul #16
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_s8_16, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 + svcntb () * 16),
	   z0 = svld2 (p0, x0 + svcntb () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_s8_m1:
**	decb	x0
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_s8_m1, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 - svcntb ()),
	   z0 = svld2 (p0, x0 - svcntb ()))

/*
** ld2_s8_m2:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_s8_m2, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 - svcntb () * 2),
	   z0 = svld2 (p0, x0 - svcntb () * 2))

/*
** ld2_s8_m16:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2_s8_m16, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 - svcntb () * 16),
	   z0 = svld2 (p0, x0 - svcntb () * 16))

/*
** ld2_s8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2_s8_m18, svint8x2_t, int8_t,
	   z0 = svld2_s8 (p0, x0 - svcntb () * 18),
	   z0 = svld2 (p0, x0 - svcntb () * 18))

/*
** ld2_vnum_s8_0:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_0, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, 0),
	   z0 = svld2_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_s8_1:
**	incb	x0
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_1, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, 1),
	   z0 = svld2_vnum (p0, x0, 1))

/*
** ld2_vnum_s8_2:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_2, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, 2),
	   z0 = svld2_vnum (p0, x0, 2))

/*
** ld2_vnum_s8_14:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_14, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, 14),
	   z0 = svld2_vnum (p0, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_s8_16:
**	incb	x0, all, mul #16
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_16, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, 16),
	   z0 = svld2_vnum (p0, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2_vnum_s8_m1:
**	decb	x0
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_m1, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, -1),
	   z0 = svld2_vnum (p0, x0, -1))

/*
** ld2_vnum_s8_m2:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_m2, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, -2),
	   z0 = svld2_vnum (p0, x0, -2))

/*
** ld2_vnum_s8_m16:
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_m16, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, -16),
	   z0 = svld2_vnum (p0, x0, -16))

/*
** ld2_vnum_s8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2_vnum_s8_m18, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, -18),
	   z0 = svld2_vnum (p0, x0, -18))

/*
** ld2_vnum_s8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld2b	{z0\.b(?: - |, )z1\.b}, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ld2_vnum_s8_x1, svint8x2_t, int8_t,
	   z0 = svld2_vnum_s8 (p0, x0, x1),
	   z0 = svld2_vnum (p0, x0, x1))
