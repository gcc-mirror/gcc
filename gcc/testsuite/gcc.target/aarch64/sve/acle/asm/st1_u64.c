/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1_u64_base:
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u64_base, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0, z0),
	    svst1 (p0, x0, z0))

/*
** st1_u64_index:
**	st1d	z0\.d, p0, \[x0, x1, lsl 3\]
**	ret
*/
TEST_STORE (st1_u64_index, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 + x1, z0),
	    svst1 (p0, x0 + x1, z0))

/*
** st1_u64_1:
**	st1d	z0\.d, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1_u64_1, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 + svcntd (), z0),
	    svst1 (p0, x0 + svcntd (), z0))

/*
** st1_u64_7:
**	st1d	z0\.d, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1_u64_7, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 + svcntd () * 7, z0),
	    svst1 (p0, x0 + svcntd () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_8:
**	incb	x0, all, mul #8
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u64_8, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 + svcntd () * 8, z0),
	    svst1 (p0, x0 + svcntd () * 8, z0))

/*
** st1_u64_m1:
**	st1d	z0\.d, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1_u64_m1, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 - svcntd (), z0),
	    svst1 (p0, x0 - svcntd (), z0))

/*
** st1_u64_m8:
**	st1d	z0\.d, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1_u64_m8, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 - svcntd () * 8, z0),
	    svst1 (p0, x0 - svcntd () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_u64_m9:
**	decb	x0, all, mul #9
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_u64_m9, svuint64_t, uint64_t,
	    svst1_u64 (p0, x0 - svcntd () * 9, z0),
	    svst1 (p0, x0 - svcntd () * 9, z0))

/*
** st1_vnum_u64_0:
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u64_0, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, 0, z0),
	    svst1_vnum (p0, x0, 0, z0))

/*
** st1_vnum_u64_1:
**	st1d	z0\.d, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u64_1, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, 1, z0),
	    svst1_vnum (p0, x0, 1, z0))

/*
** st1_vnum_u64_7:
**	st1d	z0\.d, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u64_7, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, 7, z0),
	    svst1_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_8:
**	incb	x0, all, mul #8
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u64_8, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, 8, z0),
	    svst1_vnum (p0, x0, 8, z0))

/*
** st1_vnum_u64_m1:
**	st1d	z0\.d, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u64_m1, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, -1, z0),
	    svst1_vnum (p0, x0, -1, z0))

/*
** st1_vnum_u64_m8:
**	st1d	z0\.d, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1_vnum_u64_m8, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, -8, z0),
	    svst1_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_u64_m9:
**	decb	x0, all, mul #9
**	st1d	z0\.d, p0, \[x0\]
**	ret
*/
TEST_STORE (st1_vnum_u64_m9, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, -9, z0),
	    svst1_vnum (p0, x0, -9, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st1_vnum_u64_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st1d	z0\.d, p0, \[\2\]
**	ret
*/
TEST_STORE (st1_vnum_u64_x1, svuint64_t, uint64_t,
	    svst1_vnum_u64 (p0, x0, x1, z0),
	    svst1_vnum (p0, x0, x1, z0))
