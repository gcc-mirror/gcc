/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1_u16_base:
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u16_base, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0, z0),
	    svst1 (p0, x0, z0))

/*
** st1_u16_index:
**	st1h	z0\.h, p0, \[x0, x1, lsl 1\]
**	ret
*/
TEST_STORE (st1_u16_index, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 + x1, z0),
	    svst1 (p0, x0 + x1, z0))

/*
** st1_u16_1:
**	st1h	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1_u16_1, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 + svcnth (), z0),
	    svst1 (p0, x0 + svcnth (), z0))

/*
** st1_u16_7:
**	st1h	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1_u16_7, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 + svcnth () * 7, z0),
	    svst1 (p0, x0 + svcnth () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u16_8:
**	incb	x0, all, mul #8
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u16_8, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 + svcnth () * 8, z0),
	    svst1 (p0, x0 + svcnth () * 8, z0))

/*
** st1_u16_m1:
**	st1h	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1_u16_m1, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 - svcnth (), z0),
	    svst1 (p0, x0 - svcnth (), z0))

/*
** st1_u16_m8:
**	st1h	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1_u16_m8, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 - svcnth () * 8, z0),
	    svst1 (p0, x0 - svcnth () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u16_m9:
**	decb	x0, all, mul #9
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u16_m9, svuint16_t, uint16_t,
	    svst1_u16 (p0, x0 - svcnth () * 9, z0),
	    svst1 (p0, x0 - svcnth () * 9, z0))

/*
** st1_vnum_u16_0:
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u16_0, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, 0, z0),
	    svst1_vnum (p0, x0, 0, z0))

/*
** st1_vnum_u16_1:
**	st1h	z0\.h, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u16_1, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, 1, z0),
	    svst1_vnum (p0, x0, 1, z0))

/*
** st1_vnum_u16_7:
**	st1h	z0\.h, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u16_7, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, 7, z0),
	    svst1_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u16_8:
**	incb	x0, all, mul #8
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u16_8, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, 8, z0),
	    svst1_vnum (p0, x0, 8, z0))

/*
** st1_vnum_u16_m1:
**	st1h	z0\.h, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u16_m1, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, -1, z0),
	    svst1_vnum (p0, x0, -1, z0))

/*
** st1_vnum_u16_m8:
**	st1h	z0\.h, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u16_m8, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, -8, z0),
	    svst1_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u16_m9:
**	decb	x0, all, mul #9
**	st1h	z0\.h, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u16_m9, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, -9, z0),
	    svst1_vnum (p0, x0, -9, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st1_vnum_u16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st1h	z0\.h, p0, \[\2\]
**	ret
*/
TEST_STORE (st1_vnum_u16_x1, svuint16_t, uint16_t,
	    svst1_vnum_u16 (p0, x0, x1, z0),
	    svst1_vnum (p0, x0, x1, z0))
