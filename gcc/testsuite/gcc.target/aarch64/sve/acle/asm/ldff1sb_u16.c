/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1sb_u16_base:
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u16_base, svuint16_t, int8_t,
	   z0 = svldff1sb_u16 (p0, x0),
	   z0 = svldff1sb_u16 (p0, x0))

/*
** ldff1sb_u16_index:
**	ldff1sb	z0\.h, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ldff1sb_u16_index, svuint16_t, int8_t,
	   z0 = svldff1sb_u16 (p0, x0 + x1),
	   z0 = svldff1sb_u16 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_u16_1:
**	inch	x0
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u16_1, svuint16_t, int8_t,
	   z0 = svldff1sb_u16 (p0, x0 + svcnth ()),
	   z0 = svldff1sb_u16 (p0, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_u16_m1:
**	dech	x0
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_u16_m1, svuint16_t, int8_t,
	   z0 = svldff1sb_u16 (p0, x0 - svcnth ()),
	   z0 = svldff1sb_u16 (p0, x0 - svcnth ()))

/*
** ldff1sb_vnum_u16_0:
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u16_0, svuint16_t, int8_t,
	   z0 = svldff1sb_vnum_u16 (p0, x0, 0),
	   z0 = svldff1sb_vnum_u16 (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_vnum_u16_1:
**	inch	x0
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u16_1, svuint16_t, int8_t,
	   z0 = svldff1sb_vnum_u16 (p0, x0, 1),
	   z0 = svldff1sb_vnum_u16 (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1sb_vnum_u16_m1:
**	dech	x0
**	ldff1sb	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u16_m1, svuint16_t, int8_t,
	   z0 = svldff1sb_vnum_u16 (p0, x0, -1),
	   z0 = svldff1sb_vnum_u16 (p0, x0, -1))

/*
** ldff1sb_vnum_u16_x1:
**	cnth	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldff1sb	z0\.h, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldff1sb	z0\.h, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ldff1sb_vnum_u16_x1, svuint16_t, int8_t,
	   z0 = svldff1sb_vnum_u16 (p0, x0, x1),
	   z0 = svldff1sb_vnum_u16 (p0, x0, x1))
