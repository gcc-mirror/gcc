/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1sw_s64_base:
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_s64_base, svint64_t, int32_t,
	   z0 = svldff1sw_s64 (p0, x0),
	   z0 = svldff1sw_s64 (p0, x0))

/*
** ldff1sw_s64_index:
**	ldff1sw	z0\.d, p0/z, \[x0, x1, lsl 2\]
**	ret
*/
TEST_LOAD (ldff1sw_s64_index, svint64_t, int32_t,
	   z0 = svldff1sw_s64 (p0, x0 + x1),
	   z0 = svldff1sw_s64 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sw_s64_1:
**	inch	x0
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_s64_1, svint64_t, int32_t,
	   z0 = svldff1sw_s64 (p0, x0 + svcntd ()),
	   z0 = svldff1sw_s64 (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sw_s64_m1:
**	dech	x0
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_s64_m1, svint64_t, int32_t,
	   z0 = svldff1sw_s64 (p0, x0 - svcntd ()),
	   z0 = svldff1sw_s64 (p0, x0 - svcntd ()))

/*
** ldff1sw_vnum_s64_0:
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_vnum_s64_0, svint64_t, int32_t,
	   z0 = svldff1sw_vnum_s64 (p0, x0, 0),
	   z0 = svldff1sw_vnum_s64 (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sw_vnum_s64_1:
**	inch	x0
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_vnum_s64_1, svint64_t, int32_t,
	   z0 = svldff1sw_vnum_s64 (p0, x0, 1),
	   z0 = svldff1sw_vnum_s64 (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sw_vnum_s64_m1:
**	dech	x0
**	ldff1sw	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sw_vnum_s64_m1, svint64_t, int32_t,
	   z0 = svldff1sw_vnum_s64 (p0, x0, -1),
	   z0 = svldff1sw_vnum_s64 (p0, x0, -1))

/* Using MUL to calculate an index would also be OK.  */
/*
** ldff1sw_vnum_s64_x1:
**	cnth	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ldff1sw	z0\.d, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldff1sw_vnum_s64_x1, svint64_t, int32_t,
	   z0 = svldff1sw_vnum_s64 (p0, x0, x1),
	   z0 = svldff1sw_vnum_s64 (p0, x0, x1))
