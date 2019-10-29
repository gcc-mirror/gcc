/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ldff1ub_u32_base:
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_u32_base, svuint32_t, uint8_t,
	   z0 = svldff1ub_u32 (p0, x0),
	   z0 = svldff1ub_u32 (p0, x0))

/*
** ldff1ub_u32_index:
**	ldff1b	z0\.s, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ldff1ub_u32_index, svuint32_t, uint8_t,
	   z0 = svldff1ub_u32 (p0, x0 + x1),
	   z0 = svldff1ub_u32 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1ub_u32_1:
**	incw	x0
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_u32_1, svuint32_t, uint8_t,
	   z0 = svldff1ub_u32 (p0, x0 + svcntw ()),
	   z0 = svldff1ub_u32 (p0, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1ub_u32_m1:
**	decw	x0
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_u32_m1, svuint32_t, uint8_t,
	   z0 = svldff1ub_u32 (p0, x0 - svcntw ()),
	   z0 = svldff1ub_u32 (p0, x0 - svcntw ()))

/*
** ldff1ub_vnum_u32_0:
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_vnum_u32_0, svuint32_t, uint8_t,
	   z0 = svldff1ub_vnum_u32 (p0, x0, 0),
	   z0 = svldff1ub_vnum_u32 (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1ub_vnum_u32_1:
**	incw	x0
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_vnum_u32_1, svuint32_t, uint8_t,
	   z0 = svldff1ub_vnum_u32 (p0, x0, 1),
	   z0 = svldff1ub_vnum_u32 (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1ub_vnum_u32_m1:
**	decw	x0
**	ldff1b	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1ub_vnum_u32_m1, svuint32_t, uint8_t,
	   z0 = svldff1ub_vnum_u32 (p0, x0, -1),
	   z0 = svldff1ub_vnum_u32 (p0, x0, -1))

/*
** ldff1ub_vnum_u32_x1:
**	cntw	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldff1b	z0\.s, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldff1b	z0\.s, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ldff1ub_vnum_u32_x1, svuint32_t, uint8_t,
	   z0 = svldff1ub_vnum_u32 (p0, x0, x1),
	   z0 = svldff1ub_vnum_u32 (p0, x0, x1))
