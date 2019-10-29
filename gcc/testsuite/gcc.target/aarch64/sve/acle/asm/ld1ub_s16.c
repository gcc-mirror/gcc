/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld1ub_s16_base:
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_s16_base, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0),
	   z0 = svld1ub_s16 (p0, x0))

/*
** ld1ub_s16_index:
**	ld1b	z0\.h, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1ub_s16_index, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 + x1),
	   z0 = svld1ub_s16 (p0, x0 + x1))

/*
** ld1ub_s16_1:
**	ld1b	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_s16_1, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 + svcnth ()),
	   z0 = svld1ub_s16 (p0, x0 + svcnth ()))

/*
** ld1ub_s16_7:
**	ld1b	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_s16_7, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 + svcnth () * 7),
	   z0 = svld1ub_s16 (p0, x0 + svcnth () * 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1ub_s16_8:
**	incb	x0, all, mul #4
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_s16_8, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 + svcnth () * 8),
	   z0 = svld1ub_s16 (p0, x0 + svcnth () * 8))

/*
** ld1ub_s16_m1:
**	ld1b	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_s16_m1, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 - svcnth ()),
	   z0 = svld1ub_s16 (p0, x0 - svcnth ()))

/*
** ld1ub_s16_m8:
**	ld1b	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_s16_m8, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 - svcnth () * 8),
	   z0 = svld1ub_s16 (p0, x0 - svcnth () * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1ub_s16_m9:
**	dech	x0, all, mul #9
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_s16_m9, svint16_t, uint8_t,
	   z0 = svld1ub_s16 (p0, x0 - svcnth () * 9),
	   z0 = svld1ub_s16 (p0, x0 - svcnth () * 9))

/*
** ld1ub_vnum_s16_0:
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_0, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, 0),
	   z0 = svld1ub_vnum_s16 (p0, x0, 0))

/*
** ld1ub_vnum_s16_1:
**	ld1b	z0\.h, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_1, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, 1),
	   z0 = svld1ub_vnum_s16 (p0, x0, 1))

/*
** ld1ub_vnum_s16_7:
**	ld1b	z0\.h, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_7, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, 7),
	   z0 = svld1ub_vnum_s16 (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1ub_vnum_s16_8:
**	incb	x0, all, mul #4
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_8, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, 8),
	   z0 = svld1ub_vnum_s16 (p0, x0, 8))

/*
** ld1ub_vnum_s16_m1:
**	ld1b	z0\.h, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_m1, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, -1),
	   z0 = svld1ub_vnum_s16 (p0, x0, -1))

/*
** ld1ub_vnum_s16_m8:
**	ld1b	z0\.h, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_m8, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, -8),
	   z0 = svld1ub_vnum_s16 (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1ub_vnum_s16_m9:
**	dech	x0, all, mul #9
**	ld1b	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_m9, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, -9),
	   z0 = svld1ub_vnum_s16 (p0, x0, -9))

/*
** ld1ub_vnum_s16_x1:
**	cnth	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1b	z0\.h, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1b	z0\.h, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ld1ub_vnum_s16_x1, svint16_t, uint8_t,
	   z0 = svld1ub_vnum_s16 (p0, x0, x1),
	   z0 = svld1ub_vnum_s16 (p0, x0, x1))
