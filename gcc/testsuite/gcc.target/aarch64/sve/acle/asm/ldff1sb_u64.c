/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ldff1sb_u64_base:
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u64_base, svuint64_t, int8_t,
	   z0 = svldff1sb_u64 (p0, x0),
	   z0 = svldff1sb_u64 (p0, x0))

/*
** ldff1sb_u64_index:
**	ldff1sb	z0\.d, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ldff1sb_u64_index, svuint64_t, int8_t,
	   z0 = svldff1sb_u64 (p0, x0 + x1),
	   z0 = svldff1sb_u64 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_u64_1:
**	incd	x0
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u64_1, svuint64_t, int8_t,
	   z0 = svldff1sb_u64 (p0, x0 + svcntd ()),
	   z0 = svldff1sb_u64 (p0, x0 + svcntd ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_u64_m1:
**	decd	x0
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u64_m1, svuint64_t, int8_t,
	   z0 = svldff1sb_u64 (p0, x0 - svcntd ()),
	   z0 = svldff1sb_u64 (p0, x0 - svcntd ()))

/*
** ldff1sb_vnum_u64_0:
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u64_0, svuint64_t, int8_t,
	   z0 = svldff1sb_vnum_u64 (p0, x0, 0),
	   z0 = svldff1sb_vnum_u64 (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_vnum_u64_1:
**	incd	x0
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u64_1, svuint64_t, int8_t,
	   z0 = svldff1sb_vnum_u64 (p0, x0, 1),
	   z0 = svldff1sb_vnum_u64 (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_vnum_u64_m1:
**	decd	x0
**	ldff1sb	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u64_m1, svuint64_t, int8_t,
	   z0 = svldff1sb_vnum_u64 (p0, x0, -1),
	   z0 = svldff1sb_vnum_u64 (p0, x0, -1))

/*
** ldff1sb_vnum_u64_x1:
**	cntd	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldff1sb	z0\.d, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldff1sb	z0\.d, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u64_x1, svuint64_t, int8_t,
	   z0 = svldff1sb_vnum_u64 (p0, x0, x1),
	   z0 = svldff1sb_vnum_u64 (p0, x0, x1))
