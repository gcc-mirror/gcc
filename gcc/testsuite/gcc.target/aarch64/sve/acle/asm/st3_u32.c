/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st3_u32_base:
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_u32_base, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0, z0),
	    svst3 (p0, x0, z0))

/*
** st3_u32_index:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, x1, lsl 2\]
**	ret
*/
TEST_STORE (st3_u32_index, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + x1, z0),
	    svst3 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_u32_1:
**	incb	x0
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_u32_1, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + svcntw (), z0),
	    svst3 (p0, x0 + svcntw (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_u32_2:
**	incb	x0, all, mul #2
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_u32_2, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + svcntw () * 2, z0),
	    svst3 (p0, x0 + svcntw () * 2, z0))

/*
** st3_u32_3:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_u32_3, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + svcntw () * 3, z0),
	    svst3 (p0, x0 + svcntw () * 3, z0))

/*
** st3_u32_21:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_u32_21, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + svcntw () * 21, z0),
	    svst3 (p0, x0 + svcntw () * 21, z0))

/*
** st3_u32_24:
**	addvl	(x[0-9]+), x0, #24
**	st3w	{z0\.s - z2\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_u32_24, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 + svcntw () * 24, z0),
	    svst3 (p0, x0 + svcntw () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_u32_m1:
**	decb	x0
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_u32_m1, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 - svcntw (), z0),
	    svst3 (p0, x0 - svcntw (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_u32_m2:
**	decb	x0, all, mul #2
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_u32_m2, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 - svcntw () * 2, z0),
	    svst3 (p0, x0 - svcntw () * 2, z0))

/*
** st3_u32_m3:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_u32_m3, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 - svcntw () * 3, z0),
	    svst3 (p0, x0 - svcntw () * 3, z0))

/*
** st3_u32_m24:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_u32_m24, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 - svcntw () * 24, z0),
	    svst3 (p0, x0 - svcntw () * 24, z0))

/*
** st3_u32_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3w	{z0\.s - z2\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_u32_m27, svuint32x3_t, uint32_t,
	    svst3_u32 (p0, x0 - svcntw () * 27, z0),
	    svst3 (p0, x0 - svcntw () * 27, z0))

/*
** st3_vnum_u32_0:
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_u32_0, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 0, z0),
	    svst3_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_u32_1:
**	incb	x0
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_u32_1, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 1, z0),
	    svst3_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_u32_2:
**	incb	x0, all, mul #2
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_u32_2, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 2, z0),
	    svst3_vnum (p0, x0, 2, z0))

/*
** st3_vnum_u32_3:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_u32_3, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 3, z0),
	    svst3_vnum (p0, x0, 3, z0))

/*
** st3_vnum_u32_21:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_u32_21, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 21, z0),
	    svst3_vnum (p0, x0, 21, z0))

/*
** st3_vnum_u32_24:
**	addvl	(x[0-9]+), x0, #24
**	st3w	{z0\.s - z2\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_u32_24, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, 24, z0),
	    svst3_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_u32_m1:
**	decb	x0
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_u32_m1, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, -1, z0),
	    svst3_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3_vnum_u32_m2:
**	decb	x0, all, mul #2
**	st3w	{z0\.s - z2\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3_vnum_u32_m2, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, -2, z0),
	    svst3_vnum (p0, x0, -2, z0))

/*
** st3_vnum_u32_m3:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_u32_m3, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, -3, z0),
	    svst3_vnum (p0, x0, -3, z0))

/*
** st3_vnum_u32_m24:
**	st3w	{z0\.s - z2\.s}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3_vnum_u32_m24, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, -24, z0),
	    svst3_vnum (p0, x0, -24, z0))

/*
** st3_vnum_u32_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3w	{z0\.s - z2\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3_vnum_u32_m27, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, -27, z0),
	    svst3_vnum (p0, x0, -27, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st3_vnum_u32_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st3w	{z0\.s - z2\.s}, p0, \[\2\]
**	ret
*/
TEST_STORE (st3_vnum_u32_x1, svuint32x3_t, uint32_t,
	    svst3_vnum_u32 (p0, x0, x1, z0),
	    svst3_vnum (p0, x0, x1, z0))
