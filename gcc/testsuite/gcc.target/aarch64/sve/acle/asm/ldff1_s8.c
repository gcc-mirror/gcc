/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1_s8_base:
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s8_base, svint8_t, int8_t,
	   z0 = svldff1_s8 (p0, x0),
	   z0 = svldff1 (p0, x0))

/*
** ldff1_s8_index:
**	ldff1b	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ldff1_s8_index, svint8_t, int8_t,
	   z0 = svldff1_s8 (p0, x0 + x1),
	   z0 = svldff1 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_s8_1:
**	incb	x0
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s8_1, svint8_t, int8_t,
	   z0 = svldff1_s8 (p0, x0 + svcntb ()),
	   z0 = svldff1 (p0, x0 + svcntb ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_s8_m1:
**	decb	x0
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_s8_m1, svint8_t, int8_t,
	   z0 = svldff1_s8 (p0, x0 - svcntb ()),
	   z0 = svldff1 (p0, x0 - svcntb ()))

/*
** ldff1_vnum_s8_0:
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s8_0, svint8_t, int8_t,
	   z0 = svldff1_vnum_s8 (p0, x0, 0),
	   z0 = svldff1_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_s8_1:
**	incb	x0
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s8_1, svint8_t, int8_t,
	   z0 = svldff1_vnum_s8 (p0, x0, 1),
	   z0 = svldff1_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_s8_m1:
**	decb	x0
**	ldff1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_s8_m1, svint8_t, int8_t,
	   z0 = svldff1_vnum_s8 (p0, x0, -1),
	   z0 = svldff1_vnum (p0, x0, -1))

/*
** ldff1_vnum_s8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldff1b	z0\.b, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldff1b	z0\.b, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ldff1_vnum_s8_x1, svint8_t, int8_t,
	   z0 = svldff1_vnum_s8 (p0, x0, x1),
	   z0 = svldff1_vnum (p0, x0, x1))
