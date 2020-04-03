/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1uh_s64_base:
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_s64_base, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0),
	   z0 = svld1uh_s64 (p0, x0))

/*
** ld1uh_s64_index:
**	ld1h	z0\.d, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld1uh_s64_index, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 + x1),
	   z0 = svld1uh_s64 (p0, x0 + x1))

/*
** ld1uh_s64_1:
**	ld1h	z0\.d, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_s64_1, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 + svcntd ()),
	   z0 = svld1uh_s64 (p0, x0 + svcntd ()))

/*
** ld1uh_s64_7:
**	ld1h	z0\.d, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_s64_7, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 + svcntd () * 7),
	   z0 = svld1uh_s64 (p0, x0 + svcntd () * 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1uh_s64_8:
**	incb	x0, all, mul #2
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_s64_8, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 + svcntd () * 8),
	   z0 = svld1uh_s64 (p0, x0 + svcntd () * 8))

/*
** ld1uh_s64_m1:
**	ld1h	z0\.d, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_s64_m1, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 - svcntd ()),
	   z0 = svld1uh_s64 (p0, x0 - svcntd ()))

/*
** ld1uh_s64_m8:
**	ld1h	z0\.d, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_s64_m8, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 - svcntd () * 8),
	   z0 = svld1uh_s64 (p0, x0 - svcntd () * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1uh_s64_m9:
**	decw	x0, all, mul #9
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_s64_m9, svint64_t, uint16_t,
	   z0 = svld1uh_s64 (p0, x0 - svcntd () * 9),
	   z0 = svld1uh_s64 (p0, x0 - svcntd () * 9))

/*
** ld1uh_vnum_s64_0:
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_0, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, 0),
	   z0 = svld1uh_vnum_s64 (p0, x0, 0))

/*
** ld1uh_vnum_s64_1:
**	ld1h	z0\.d, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_1, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, 1),
	   z0 = svld1uh_vnum_s64 (p0, x0, 1))

/*
** ld1uh_vnum_s64_7:
**	ld1h	z0\.d, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_7, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, 7),
	   z0 = svld1uh_vnum_s64 (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1uh_vnum_s64_8:
**	incb	x0, all, mul #2
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_8, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, 8),
	   z0 = svld1uh_vnum_s64 (p0, x0, 8))

/*
** ld1uh_vnum_s64_m1:
**	ld1h	z0\.d, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_m1, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, -1),
	   z0 = svld1uh_vnum_s64 (p0, x0, -1))

/*
** ld1uh_vnum_s64_m8:
**	ld1h	z0\.d, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_m8, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, -8),
	   z0 = svld1uh_vnum_s64 (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1uh_vnum_s64_m9:
**	decw	x0, all, mul #9
**	ld1h	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_m9, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, -9),
	   z0 = svld1uh_vnum_s64 (p0, x0, -9))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld1uh_vnum_s64_x1:
**	cntw	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld1h	z0\.d, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld1uh_vnum_s64_x1, svint64_t, uint16_t,
	   z0 = svld1uh_vnum_s64 (p0, x0, x1),
	   z0 = svld1uh_vnum_s64 (p0, x0, x1))
