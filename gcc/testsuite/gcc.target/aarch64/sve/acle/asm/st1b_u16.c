/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1b_u16_base:
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u16_base, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0, z0),
	    svst1b (p0, x0, z0))

/*
** st1b_u16_index:
**	st1b	z0\.h, p0, \[x0, x1\]
**	ret
*/
TEST_STORE (st1b_u16_index, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 + x1, z0),
	    svst1b (p0, x0 + x1, z0))

/*
** st1b_u16_1:
**	st1b	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1b_u16_1, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 + svcnth (), z0),
	    svst1b (p0, x0 + svcnth (), z0))

/*
** st1b_u16_7:
**	st1b	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1b_u16_7, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 + svcnth () * 7, z0),
	    svst1b (p0, x0 + svcnth () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_u16_8:
**	incb	x0, all, mul #4
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u16_8, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 + svcnth () * 8, z0),
	    svst1b (p0, x0 + svcnth () * 8, z0))

/*
** st1b_u16_m1:
**	st1b	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1b_u16_m1, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 - svcnth (), z0),
	    svst1b (p0, x0 - svcnth (), z0))

/*
** st1b_u16_m8:
**	st1b	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1b_u16_m8, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 - svcnth () * 8, z0),
	    svst1b (p0, x0 - svcnth () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_u16_m9:
**	dech	x0, all, mul #9
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u16_m9, svuint16_t, uint8_t,
	    svst1b_u16 (p0, x0 - svcnth () * 9, z0),
	    svst1b (p0, x0 - svcnth () * 9, z0))

/*
** st1b_vnum_u16_0:
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_0, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, 0, z0),
	    svst1b_vnum (p0, x0, 0, z0))

/*
** st1b_vnum_u16_1:
**	st1b	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_1, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, 1, z0),
	    svst1b_vnum (p0, x0, 1, z0))

/*
** st1b_vnum_u16_7:
**	st1b	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_7, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, 7, z0),
	    svst1b_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_vnum_u16_8:
**	incb	x0, all, mul #4
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_8, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, 8, z0),
	    svst1b_vnum (p0, x0, 8, z0))

/*
** st1b_vnum_u16_m1:
**	st1b	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_m1, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, -1, z0),
	    svst1b_vnum (p0, x0, -1, z0))

/*
** st1b_vnum_u16_m8:
**	st1b	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_m8, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, -8, z0),
	    svst1b_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_vnum_u16_m9:
**	dech	x0, all, mul #9
**	st1b	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u16_m9, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, -9, z0),
	    svst1b_vnum (p0, x0, -9, z0))

/*
** st1b_vnum_u16_x1:
**	cnth	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1b	z0\.h, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1b	z0\.h, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st1b_vnum_u16_x1, svuint16_t, uint8_t,
	    svst1b_vnum_u16 (p0, x0, x1, z0),
	    svst1b_vnum (p0, x0, x1, z0))
