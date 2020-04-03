/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1_s64_base:
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_s64_base, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0),
	   z0 = svldnt1 (p0, x0))

/*
** ldnt1_s64_index:
**	ldnt1d	z0\.d, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ldnt1_s64_index, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 + x1),
	   z0 = svldnt1 (p0, x0 + x1))

/*
** ldnt1_s64_1:
**	ldnt1d	z0\.d, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_s64_1, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 + svcntd ()),
	   z0 = svldnt1 (p0, x0 + svcntd ()))

/*
** ldnt1_s64_7:
**	ldnt1d	z0\.d, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_s64_7, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 + svcntd () * 7),
	   z0 = svldnt1 (p0, x0 + svcntd () * 7))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s64_8:
**	incb	x0, all, mul #8
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_s64_8, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 + svcntd () * 8),
	   z0 = svldnt1 (p0, x0 + svcntd () * 8))

/*
** ldnt1_s64_m1:
**	ldnt1d	z0\.d, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_s64_m1, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 - svcntd ()),
	   z0 = svldnt1 (p0, x0 - svcntd ()))

/*
** ldnt1_s64_m8:
**	ldnt1d	z0\.d, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_s64_m8, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 - svcntd () * 8),
	   z0 = svldnt1 (p0, x0 - svcntd () * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_s64_m9:
**	decb	x0, all, mul #9
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_s64_m9, svint64_t, int64_t,
	   z0 = svldnt1_s64 (p0, x0 - svcntd () * 9),
	   z0 = svldnt1 (p0, x0 - svcntd () * 9))

/*
** ldnt1_vnum_s64_0:
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_0, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, 0),
	   z0 = svldnt1_vnum (p0, x0, 0))

/*
** ldnt1_vnum_s64_1:
**	ldnt1d	z0\.d, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_1, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, 1),
	   z0 = svldnt1_vnum (p0, x0, 1))

/*
** ldnt1_vnum_s64_7:
**	ldnt1d	z0\.d, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_7, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, 7),
	   z0 = svldnt1_vnum (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s64_8:
**	incb	x0, all, mul #8
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_8, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, 8),
	   z0 = svldnt1_vnum (p0, x0, 8))

/*
** ldnt1_vnum_s64_m1:
**	ldnt1d	z0\.d, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_m1, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, -1),
	   z0 = svldnt1_vnum (p0, x0, -1))

/*
** ldnt1_vnum_s64_m8:
**	ldnt1d	z0\.d, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_m8, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, -8),
	   z0 = svldnt1_vnum (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_s64_m9:
**	decb	x0, all, mul #9
**	ldnt1d	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_m9, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, -9),
	   z0 = svldnt1_vnum (p0, x0, -9))

/* Using MUL to calculate an index would also be OK.  */
/*
** ldnt1_vnum_s64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ldnt1d	z0\.d, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldnt1_vnum_s64_x1, svint64_t, int64_t,
	   z0 = svldnt1_vnum_s64 (p0, x0, x1),
	   z0 = svldnt1_vnum (p0, x0, x1))
