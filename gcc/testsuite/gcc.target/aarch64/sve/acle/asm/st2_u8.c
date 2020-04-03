/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st2_u8_base:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u8_base, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0, z0),
	    svst2 (p0, x0, z0))

/*
** st2_u8_index:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, x1\]
**	ret
*/
TEST_STORE (st2_u8_index, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 + x1, z0),
	    svst2 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u8_1:
**	incb	x0
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u8_1, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 + svcntb (), z0),
	    svst2 (p0, x0 + svcntb (), z0))

/*
** st2_u8_2:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_u8_2, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 + svcntb () * 2, z0),
	    svst2 (p0, x0 + svcntb () * 2, z0))

/*
** st2_u8_14:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_u8_14, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 + svcntb () * 14, z0),
	    svst2 (p0, x0 + svcntb () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u8_16:
**	incb	x0, all, mul #16
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u8_16, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 + svcntb () * 16, z0),
	    svst2 (p0, x0 + svcntb () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_u8_m1:
**	decb	x0
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_u8_m1, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 - svcntb (), z0),
	    svst2 (p0, x0 - svcntb (), z0))

/*
** st2_u8_m2:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_u8_m2, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 - svcntb () * 2, z0),
	    svst2 (p0, x0 - svcntb () * 2, z0))

/*
** st2_u8_m16:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_u8_m16, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 - svcntb () * 16, z0),
	    svst2 (p0, x0 - svcntb () * 16, z0))

/*
** st2_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_u8_m18, svuint8x2_t, uint8_t,
	    svst2_u8 (p0, x0 - svcntb () * 18, z0),
	    svst2 (p0, x0 - svcntb () * 18, z0))

/*
** st2_vnum_u8_0:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u8_0, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, 0, z0),
	    svst2_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u8_1:
**	incb	x0
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u8_1, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, 1, z0),
	    svst2_vnum (p0, x0, 1, z0))

/*
** st2_vnum_u8_2:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u8_2, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, 2, z0),
	    svst2_vnum (p0, x0, 2, z0))

/*
** st2_vnum_u8_14:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u8_14, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, 14, z0),
	    svst2_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u8_16:
**	incb	x0, all, mul #16
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u8_16, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, 16, z0),
	    svst2_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_u8_m1:
**	decb	x0
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_u8_m1, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, -1, z0),
	    svst2_vnum (p0, x0, -1, z0))

/*
** st2_vnum_u8_m2:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u8_m2, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, -2, z0),
	    svst2_vnum (p0, x0, -2, z0))

/*
** st2_vnum_u8_m16:
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_u8_m16, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, -16, z0),
	    svst2_vnum (p0, x0, -16, z0))

/*
** st2_vnum_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_vnum_u8_m18, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, -18, z0),
	    svst2_vnum (p0, x0, -18, z0))

/*
** st2_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st2b	{z0\.b(?: - |, )z1\.b}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st2_vnum_u8_x1, svuint8x2_t, uint8_t,
	    svst2_vnum_u8 (p0, x0, x1, z0),
	    svst2_vnum (p0, x0, x1, z0))
