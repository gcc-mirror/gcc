/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1_u8_base:
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_u8_base, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0),
	   z0 = svld1 (p0, x0))

/*
** ld1_u8_index:
**	ld1b	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1_u8_index, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 + x1),
	   z0 = svld1 (p0, x0 + x1))

/*
** ld1_u8_1:
**	ld1b	z0\.b, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_u8_1, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 + svcntb ()),
	   z0 = svld1 (p0, x0 + svcntb ()))

/*
** ld1_u8_7:
**	ld1b	z0\.b, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1_u8_7, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 + svcntb () * 7),
	   z0 = svld1 (p0, x0 + svcntb () * 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_8:
**	incb	x0, all, mul #8
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_u8_8, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 + svcntb () * 8),
	   z0 = svld1 (p0, x0 + svcntb () * 8))

/*
** ld1_u8_m1:
**	ld1b	z0\.b, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_u8_m1, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 - svcntb ()),
	   z0 = svld1 (p0, x0 - svcntb ()))

/*
** ld1_u8_m8:
**	ld1b	z0\.b, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1_u8_m8, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 - svcntb () * 8),
	   z0 = svld1 (p0, x0 - svcntb () * 8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_u8_m9:
**	decb	x0, all, mul #9
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_u8_m9, svuint8_t, uint8_t,
	   z0 = svld1_u8 (p0, x0 - svcntb () * 9),
	   z0 = svld1 (p0, x0 - svcntb () * 9))

/*
** ld1_vnum_u8_0:
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_0, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, 0),
	   z0 = svld1_vnum (p0, x0, 0))

/*
** ld1_vnum_u8_1:
**	ld1b	z0\.b, p0/z, \[x0, #1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_1, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, 1),
	   z0 = svld1_vnum (p0, x0, 1))

/*
** ld1_vnum_u8_7:
**	ld1b	z0\.b, p0/z, \[x0, #7, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_7, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, 7),
	   z0 = svld1_vnum (p0, x0, 7))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_8:
**	incb	x0, all, mul #8
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_8, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, 8),
	   z0 = svld1_vnum (p0, x0, 8))

/*
** ld1_vnum_u8_m1:
**	ld1b	z0\.b, p0/z, \[x0, #-1, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_m1, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, -1),
	   z0 = svld1_vnum (p0, x0, -1))

/*
** ld1_vnum_u8_m8:
**	ld1b	z0\.b, p0/z, \[x0, #-8, mul vl\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_m8, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, -8),
	   z0 = svld1_vnum (p0, x0, -8))

/* Moving the constant into a register would also be OK.  */
/*
** ld1_vnum_u8_m9:
**	decb	x0, all, mul #9
**	ld1b	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1_vnum_u8_m9, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, -9),
	   z0 = svld1_vnum (p0, x0, -9))

/*
** ld1_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld1b	z0\.b, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld1b	z0\.b, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ld1_vnum_u8_x1, svuint8_t, uint8_t,
	   z0 = svld1_vnum_u8 (p0, x0, x1),
	   z0 = svld1_vnum (p0, x0, x1))
