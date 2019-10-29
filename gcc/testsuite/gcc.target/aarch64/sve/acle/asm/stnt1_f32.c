/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** stnt1_f32_base:
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_f32_base, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0, z0),
	    svstnt1 (p0, x0, z0))

/*
** stnt1_f32_index:
**	stnt1w	z0\.s, p0, \[x0, x1, lsl 2\]
**	ret
*/
TEST_STORE (stnt1_f32_index, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 + x1, z0),
	    svstnt1 (p0, x0 + x1, z0))

/*
** stnt1_f32_1:
**	stnt1w	z0\.s, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_f32_1, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 + svcntw (), z0),
	    svstnt1 (p0, x0 + svcntw (), z0))

/*
** stnt1_f32_7:
**	stnt1w	z0\.s, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_f32_7, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 + svcntw () * 7, z0),
	    svstnt1 (p0, x0 + svcntw () * 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_8:
**	incb	x0, all, mul #8
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_f32_8, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 + svcntw () * 8, z0),
	    svstnt1 (p0, x0 + svcntw () * 8, z0))

/*
** stnt1_f32_m1:
**	stnt1w	z0\.s, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_f32_m1, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 - svcntw (), z0),
	    svstnt1 (p0, x0 - svcntw (), z0))

/*
** stnt1_f32_m8:
**	stnt1w	z0\.s, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_f32_m8, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 - svcntw () * 8, z0),
	    svstnt1 (p0, x0 - svcntw () * 8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_m9:
**	decb	x0, all, mul #9
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_f32_m9, svfloat32_t, float32_t,
	    svstnt1_f32 (p0, x0 - svcntw () * 9, z0),
	    svstnt1 (p0, x0 - svcntw () * 9, z0))

/*
** stnt1_vnum_f32_0:
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_0, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, 0, z0),
	    svstnt1_vnum (p0, x0, 0, z0))

/*
** stnt1_vnum_f32_1:
**	stnt1w	z0\.s, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_1, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, 1, z0),
	    svstnt1_vnum (p0, x0, 1, z0))

/*
** stnt1_vnum_f32_7:
**	stnt1w	z0\.s, p0, \[x0, #7, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_7, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, 7, z0),
	    svstnt1_vnum (p0, x0, 7, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_8:
**	incb	x0, all, mul #8
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_8, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, 8, z0),
	    svstnt1_vnum (p0, x0, 8, z0))

/*
** stnt1_vnum_f32_m1:
**	stnt1w	z0\.s, p0, \[x0, #-1, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_m1, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, -1, z0),
	    svstnt1_vnum (p0, x0, -1, z0))

/*
** stnt1_vnum_f32_m8:
**	stnt1w	z0\.s, p0, \[x0, #-8, mul vl\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_m8, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, -8, z0),
	    svstnt1_vnum (p0, x0, -8, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_m9:
**	decb	x0, all, mul #9
**	stnt1w	z0\.s, p0, \[x0\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_m9, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, -9, z0),
	    svstnt1_vnum (p0, x0, -9, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** stnt1_vnum_f32_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	stnt1w	z0\.s, p0, \[\2\]
**	ret
*/
TEST_STORE (stnt1_vnum_f32_x1, svfloat32_t, float32_t,
	    svstnt1_vnum_f32 (p0, x0, x1, z0),
	    svstnt1_vnum (p0, x0, x1, z0))
