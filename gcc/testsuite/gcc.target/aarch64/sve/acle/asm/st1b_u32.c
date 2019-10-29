/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1b_u32_base:
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u32_base, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0, z0),
	    svst1b (p0, x0, z0))

/*
** st1b_u32_index:
**	st1b	z0\.s, p0, \[x0, x1\]
**	ret
*/
TEST_STORE (st1b_u32_index, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 + x1, z0),
	    svst1b (p0, x0 + x1, z0))

/*
** st1b_u32_1:
**	st1b	z0\.s, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1b_u32_1, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 + svcntw (), z0),
	    svst1b (p0, x0 + svcntw (), z0))

/*
** st1b_u32_7:
**	st1b	z0\.s, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1b_u32_7, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 + svcntw () * 7, z0),
	    svst1b (p0, x0 + svcntw () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_u32_8:
**	incb	x0, all, mul #2
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u32_8, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 + svcntw () * 8, z0),
	    svst1b (p0, x0 + svcntw () * 8, z0))

/*
** st1b_u32_m1:
**	st1b	z0\.s, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1b_u32_m1, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 - svcntw (), z0),
	    svst1b (p0, x0 - svcntw (), z0))

/*
** st1b_u32_m8:
**	st1b	z0\.s, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1b_u32_m8, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 - svcntw () * 8, z0),
	    svst1b (p0, x0 - svcntw () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_u32_m9:
**	decw	x0, all, mul #9
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_u32_m9, svuint32_t, uint8_t,
	    svst1b_u32 (p0, x0 - svcntw () * 9, z0),
	    svst1b (p0, x0 - svcntw () * 9, z0))

/*
** st1b_vnum_u32_0:
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_0, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, 0, z0),
	    svst1b_vnum (p0, x0, 0, z0))

/*
** st1b_vnum_u32_1:
**	st1b	z0\.s, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_1, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, 1, z0),
	    svst1b_vnum (p0, x0, 1, z0))

/*
** st1b_vnum_u32_7:
**	st1b	z0\.s, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_7, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, 7, z0),
	    svst1b_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_vnum_u32_8:
**	incb	x0, all, mul #2
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_8, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, 8, z0),
	    svst1b_vnum (p0, x0, 8, z0))

/*
** st1b_vnum_u32_m1:
**	st1b	z0\.s, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_m1, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, -1, z0),
	    svst1b_vnum (p0, x0, -1, z0))

/*
** st1b_vnum_u32_m8:
**	st1b	z0\.s, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_m8, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, -8, z0),
	    svst1b_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1b_vnum_u32_m9:
**	decw	x0, all, mul #9
**	st1b	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (st1b_vnum_u32_m9, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, -9, z0),
	    svst1b_vnum (p0, x0, -9, z0))

/*
** st1b_vnum_u32_x1:
**	cntw	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1b	z0\.s, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1b	z0\.s, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st1b_vnum_u32_x1, svuint32_t, uint8_t,
	    svst1b_vnum_u32 (p0, x0, x1, z0),
	    svst1b_vnum (p0, x0, x1, z0))
