/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1_s16_base:
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_s16_base, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0),
	   z0 = svld1 (p0, x0))

/*
** ld1_s16_index:
**	ld1h	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld1_s16_index, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 + x1),
	   z0 = svld1 (p0, x0 + x1))

/*
** ld1_s16_1:
**	ld1h	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_s16_1, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 + svcnth ()),
	   z0 = svld1 (p0, x0 + svcnth ()))

/*
** ld1_s16_7:
**	ld1h	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1_s16_7, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 + svcnth () * 7),
	   z0 = svld1 (p0, x0 + svcnth () * 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s16_8:
**	incb	x0, all, mul #8
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_s16_8, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 + svcnth () * 8),
	   z0 = svld1 (p0, x0 + svcnth () * 8))

/*
** ld1_s16_m1:
**	ld1h	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_s16_m1, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 - svcnth ()),
	   z0 = svld1 (p0, x0 - svcnth ()))

/*
** ld1_s16_m8:
**	ld1h	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1_s16_m8, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 - svcnth () * 8),
	   z0 = svld1 (p0, x0 - svcnth () * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_s16_m9:
**	decb	x0, all, mul #9
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_s16_m9, svint16_t, int16_t,
	   z0 = svld1_s16 (p0, x0 - svcnth () * 9),
	   z0 = svld1 (p0, x0 - svcnth () * 9))

/*
** ld1_vnum_s16_0:
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_0, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, 0),
	   z0 = svld1_vnum (p0, x0, 0))

/*
** ld1_vnum_s16_1:
**	ld1h	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_1, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, 1),
	   z0 = svld1_vnum (p0, x0, 1))

/*
** ld1_vnum_s16_7:
**	ld1h	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_7, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, 7),
	   z0 = svld1_vnum (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s16_8:
**	incb	x0, all, mul #8
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_8, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, 8),
	   z0 = svld1_vnum (p0, x0, 8))

/*
** ld1_vnum_s16_m1:
**	ld1h	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_m1, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, -1),
	   z0 = svld1_vnum (p0, x0, -1))

/*
** ld1_vnum_s16_m8:
**	ld1h	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_m8, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, -8),
	   z0 = svld1_vnum (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_s16_m9:
**	decb	x0, all, mul #9
**	ld1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_m9, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, -9),
	   z0 = svld1_vnum (p0, x0, -9))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld1_vnum_s16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld1h	z0\.h, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld1_vnum_s16_x1, svint16_t, int16_t,
	   z0 = svld1_vnum_s16 (p0, x0, x1),
	   z0 = svld1_vnum (p0, x0, x1))
