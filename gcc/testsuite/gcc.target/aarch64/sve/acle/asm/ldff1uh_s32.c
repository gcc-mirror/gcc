/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1uh_s32_base:
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_s32_base, svint32_t, uint16_t,
	   z0 = svldff1uh_s32 (p0, x0),
	   z0 = svldff1uh_s32 (p0, x0))

/*
** ldff1uh_s32_index:
**	ldff1h	z0\.s, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ldff1uh_s32_index, svint32_t, uint16_t,
	   z0 = svldff1uh_s32 (p0, x0 + x1),
	   z0 = svldff1uh_s32 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1uh_s32_1:
**	inch	x0
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_s32_1, svint32_t, uint16_t,
	   z0 = svldff1uh_s32 (p0, x0 + svcntw ()),
	   z0 = svldff1uh_s32 (p0, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1uh_s32_m1:
**	dech	x0
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_s32_m1, svint32_t, uint16_t,
	   z0 = svldff1uh_s32 (p0, x0 - svcntw ()),
	   z0 = svldff1uh_s32 (p0, x0 - svcntw ()))

/*
** ldff1uh_vnum_s32_0:
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_vnum_s32_0, svint32_t, uint16_t,
	   z0 = svldff1uh_vnum_s32 (p0, x0, 0),
	   z0 = svldff1uh_vnum_s32 (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1uh_vnum_s32_1:
**	inch	x0
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_vnum_s32_1, svint32_t, uint16_t,
	   z0 = svldff1uh_vnum_s32 (p0, x0, 1),
	   z0 = svldff1uh_vnum_s32 (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1uh_vnum_s32_m1:
**	dech	x0
**	ldff1h	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1uh_vnum_s32_m1, svint32_t, uint16_t,
	   z0 = svldff1uh_vnum_s32 (p0, x0, -1),
	   z0 = svldff1uh_vnum_s32 (p0, x0, -1))

/* Using MUL to calculate an index would also be OK.  */
/*
** ldff1uh_vnum_s32_x1:
**	cnth	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ldff1h	z0\.s, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldff1uh_vnum_s32_x1, svint32_t, uint16_t,
	   z0 = svldff1uh_vnum_s32 (p0, x0, x1),
	   z0 = svldff1uh_vnum_s32 (p0, x0, x1))
