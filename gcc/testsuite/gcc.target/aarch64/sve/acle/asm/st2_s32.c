/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st2_s32_base:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_s32_base, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0, z0),
	    svst2 (p0, x0, z0))

/*
** st2_s32_index:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, x1, lsl 2\]
**	ret
*/
TEST_STORE (st2_s32_index, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 + x1, z0),
	    svst2 (p0, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_s32_1:
**	incb	x0
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_s32_1, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 + svcntw (), z0),
	    svst2 (p0, x0 + svcntw (), z0))

/*
** st2_s32_2:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_s32_2, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 + svcntw () * 2, z0),
	    svst2 (p0, x0 + svcntw () * 2, z0))

/*
** st2_s32_14:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_s32_14, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 + svcntw () * 14, z0),
	    svst2 (p0, x0 + svcntw () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_s32_16:
**	incb	x0, all, mul #16
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_s32_16, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 + svcntw () * 16, z0),
	    svst2 (p0, x0 + svcntw () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_s32_m1:
**	decb	x0
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_s32_m1, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 - svcntw (), z0),
	    svst2 (p0, x0 - svcntw (), z0))

/*
** st2_s32_m2:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_s32_m2, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 - svcntw () * 2, z0),
	    svst2 (p0, x0 - svcntw () * 2, z0))

/*
** st2_s32_m16:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_s32_m16, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 - svcntw () * 16, z0),
	    svst2 (p0, x0 - svcntw () * 16, z0))

/*
** st2_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_s32_m18, svint32x2_t, int32_t,
	    svst2_s32 (p0, x0 - svcntw () * 18, z0),
	    svst2 (p0, x0 - svcntw () * 18, z0))

/*
** st2_vnum_s32_0:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_s32_0, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, 0, z0),
	    svst2_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_s32_1:
**	incb	x0
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_s32_1, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, 1, z0),
	    svst2_vnum (p0, x0, 1, z0))

/*
** st2_vnum_s32_2:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_s32_2, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, 2, z0),
	    svst2_vnum (p0, x0, 2, z0))

/*
** st2_vnum_s32_14:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_s32_14, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, 14, z0),
	    svst2_vnum (p0, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_s32_16:
**	incb	x0, all, mul #16
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_s32_16, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, 16, z0),
	    svst2_vnum (p0, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st2_vnum_s32_m1:
**	decb	x0
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0\]
**	ret
*/
TEST_STORE (st2_vnum_s32_m1, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, -1, z0),
	    svst2_vnum (p0, x0, -1, z0))

/*
** st2_vnum_s32_m2:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_s32_m2, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, -2, z0),
	    svst2_vnum (p0, x0, -2, z0))

/*
** st2_vnum_s32_m16:
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE (st2_vnum_s32_m16, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, -16, z0),
	    svst2_vnum (p0, x0, -16, z0))

/*
** st2_vnum_s32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[\1\]
**	ret
*/
TEST_STORE (st2_vnum_s32_m18, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, -18, z0),
	    svst2_vnum (p0, x0, -18, z0))

/* Using MUL to calculate an index would also be OK.  */
/*
** st2_vnum_s32_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	st2w	{z0\.s(?: - |, )z1\.s}, p0, \[\2\]
**	ret
*/
TEST_STORE (st2_vnum_s32_x1, svint32x2_t, int32_t,
	    svst2_vnum_s32 (p0, x0, x1, z0),
	    svst2_vnum (p0, x0, x1, z0))
