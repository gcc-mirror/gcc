/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ldff1_s16_base:
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s16_base, svint16_t, int16_t,
	   z0 = svldff1_s16 (p0, x0),
	   z0 = svldff1 (p0, x0))

/*
** ldff1_s16_index:
**	ldff1h	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ldff1_s16_index, svint16_t, int16_t,
	   z0 = svldff1_s16 (p0, x0 + x1),
	   z0 = svldff1 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_s16_1:
**	incb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s16_1, svint16_t, int16_t,
	   z0 = svldff1_s16 (p0, x0 + svcnth ()),
	   z0 = svldff1 (p0, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_s16_m1:
**	decb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s16_m1, svint16_t, int16_t,
	   z0 = svldff1_s16 (p0, x0 - svcnth ()),
	   z0 = svldff1 (p0, x0 - svcnth ()))

/*
** ldff1_vnum_s16_0:
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s16_0, svint16_t, int16_t,
	   z0 = svldff1_vnum_s16 (p0, x0, 0),
	   z0 = svldff1_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_s16_1:
**	incb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s16_1, svint16_t, int16_t,
	   z0 = svldff1_vnum_s16 (p0, x0, 1),
	   z0 = svldff1_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_s16_m1:
**	decb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s16_m1, svint16_t, int16_t,
	   z0 = svldff1_vnum_s16 (p0, x0, -1),
	   z0 = svldff1_vnum (p0, x0, -1))

/* Using MUL to calculate an index would also be OK.  */
/*
** ldff1_vnum_s16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ldff1h	z0\.h, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s16_x1, svint16_t, int16_t,
	   z0 = svldff1_vnum_s16 (p0, x0, x1),
	   z0 = svldff1_vnum (p0, x0, x1))
