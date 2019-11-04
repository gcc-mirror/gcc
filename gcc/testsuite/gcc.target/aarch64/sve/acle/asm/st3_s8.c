/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st3_s8_base:
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s8_base, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0, z0),
	    svst3 (p0, x0, z0))

/*
** st3_s8_index:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, x1\]
**	ret
*/
TEST_STORE (st3_s8_index, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + x1, z0),
	    svst3 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s8_1:
**	incb	x0
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s8_1, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + svcntb (), z0),
	    svst3 (p0, x0 + svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s8_2:
**	incb	x0, all, mul #2
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s8_2, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + svcntb () * 2, z0),
	    svst3 (p0, x0 + svcntb () * 2, z0))

/*
** st3_s8_3:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_s8_3, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + svcntb () * 3, z0),
	    svst3 (p0, x0 + svcntb () * 3, z0))

/*
** st3_s8_21:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_s8_21, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + svcntb () * 21, z0),
	    svst3 (p0, x0 + svcntb () * 21, z0))

/*
** st3_s8_24:
**	addvl	(x[0-9]+), x0, #24
**	st3b	{z0\.b - z2\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_s8_24, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 + svcntb () * 24, z0),
	    svst3 (p0, x0 + svcntb () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s8_m1:
**	decb	x0
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s8_m1, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 - svcntb (), z0),
	    svst3 (p0, x0 - svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_s8_m2:
**	decb	x0, all, mul #2
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_s8_m2, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 - svcntb () * 2, z0),
	    svst3 (p0, x0 - svcntb () * 2, z0))

/*
** st3_s8_m3:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_s8_m3, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 - svcntb () * 3, z0),
	    svst3 (p0, x0 - svcntb () * 3, z0))

/*
** st3_s8_m24:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_s8_m24, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 - svcntb () * 24, z0),
	    svst3 (p0, x0 - svcntb () * 24, z0))

/*
** st3_s8_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3b	{z0\.b - z2\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_s8_m27, svint8x3_t, int8_t,
	    svst3_s8 (p0, x0 - svcntb () * 27, z0),
	    svst3 (p0, x0 - svcntb () * 27, z0))

/*
** st3_vnum_s8_0:
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s8_0, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 0, z0),
	    svst3_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s8_1:
**	incb	x0
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s8_1, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 1, z0),
	    svst3_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s8_2:
**	incb	x0, all, mul #2
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s8_2, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 2, z0),
	    svst3_vnum (p0, x0, 2, z0))

/*
** st3_vnum_s8_3:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s8_3, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 3, z0),
	    svst3_vnum (p0, x0, 3, z0))

/*
** st3_vnum_s8_21:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s8_21, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 21, z0),
	    svst3_vnum (p0, x0, 21, z0))

/*
** st3_vnum_s8_24:
**	addvl	(x[0-9]+), x0, #24
**	st3b	{z0\.b - z2\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_s8_24, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, 24, z0),
	    svst3_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s8_m1:
**	decb	x0
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s8_m1, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, -1, z0),
	    svst3_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_s8_m2:
**	decb	x0, all, mul #2
**	st3b	{z0\.b - z2\.b}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_s8_m2, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, -2, z0),
	    svst3_vnum (p0, x0, -2, z0))

/*
** st3_vnum_s8_m3:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s8_m3, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, -3, z0),
	    svst3_vnum (p0, x0, -3, z0))

/*
** st3_vnum_s8_m24:
**	st3b	{z0\.b - z2\.b}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_s8_m24, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, -24, z0),
	    svst3_vnum (p0, x0, -24, z0))

/*
** st3_vnum_s8_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3b	{z0\.b - z2\.b}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_s8_m27, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, -27, z0),
	    svst3_vnum (p0, x0, -27, z0))

/*
** st3_vnum_s8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st3b	{z0\.b - z2\.b}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st3b	{z0\.b - z2\.b}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st3_vnum_s8_x1, svint8x3_t, int8_t,
	    svst3_vnum_s8 (p0, x0, x1, z0),
	    svst3_vnum (p0, x0, x1, z0))
