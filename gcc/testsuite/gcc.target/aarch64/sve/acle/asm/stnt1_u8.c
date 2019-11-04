/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** stnt1_u8_base:
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_u8_base, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0, z0),
	    svstnt1 (p0, x0, z0))

/*
** stnt1_u8_index:
**	stnt1b	z0\.b, p0, \[x0, x1\]
**	ret
*/
TEST_STORE (stnt1_u8_index, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 + x1, z0),
	    svstnt1 (p0, x0 + x1, z0))

/*
** stnt1_u8_1:
**	stnt1b	z0\.b, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_u8_1, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 + svcntb (), z0),
	    svstnt1 (p0, x0 + svcntb (), z0))

/*
** stnt1_u8_7:
**	stnt1b	z0\.b, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_u8_7, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 + svcntb () * 7, z0),
	    svstnt1 (p0, x0 + svcntb () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u8_8:
**	incb	x0, all, mul #8
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_u8_8, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 + svcntb () * 8, z0),
	    svstnt1 (p0, x0 + svcntb () * 8, z0))

/*
** stnt1_u8_m1:
**	stnt1b	z0\.b, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_u8_m1, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 - svcntb (), z0),
	    svstnt1 (p0, x0 - svcntb (), z0))

/*
** stnt1_u8_m8:
**	stnt1b	z0\.b, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_u8_m8, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 - svcntb () * 8, z0),
	    svstnt1 (p0, x0 - svcntb () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_u8_m9:
**	decb	x0, all, mul #9
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_u8_m9, svuint8_t, uint8_t,
	    svstnt1_u8 (p0, x0 - svcntb () * 9, z0),
	    svstnt1 (p0, x0 - svcntb () * 9, z0))

/*
** stnt1_vnum_u8_0:
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_0, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, 0, z0),
	    svstnt1_vnum (p0, x0, 0, z0))

/*
** stnt1_vnum_u8_1:
**	stnt1b	z0\.b, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_1, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, 1, z0),
	    svstnt1_vnum (p0, x0, 1, z0))

/*
** stnt1_vnum_u8_7:
**	stnt1b	z0\.b, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_7, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, 7, z0),
	    svstnt1_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u8_8:
**	incb	x0, all, mul #8
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_8, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, 8, z0),
	    svstnt1_vnum (p0, x0, 8, z0))

/*
** stnt1_vnum_u8_m1:
**	stnt1b	z0\.b, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_m1, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, -1, z0),
	    svstnt1_vnum (p0, x0, -1, z0))

/*
** stnt1_vnum_u8_m8:
**	stnt1b	z0\.b, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_m8, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, -8, z0),
	    svstnt1_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_u8_m9:
**	decb	x0, all, mul #9
**	stnt1b	z0\.b, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_u8_m9, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, -9, z0),
	    svstnt1_vnum (p0, x0, -9, z0))

/*
** stnt1_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1b	z0\.b, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1b	z0\.b, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (stnt1_vnum_u8_x1, svuint8_t, uint8_t,
	    svstnt1_vnum_u8 (p0, x0, x1, z0),
	    svstnt1_vnum (p0, x0, x1, z0))
