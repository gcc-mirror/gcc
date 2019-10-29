/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld4_s32_base:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_base, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0),
	   z0 = svld4 (p0, x0))

/*
** ld4_s32_index:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, x1, lsl 2\]
**	ret
*/
TEST_LOAD (ld4_s32_index, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + x1),
	   z0 = svld4 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_1:
**	incb	x0
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_1, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw ()),
	   z0 = svld4 (p0, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_2:
**	incb	x0, all, mul #2
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_2, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw () * 2),
	   z0 = svld4 (p0, x0 + svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_3:
**	incb	x0, all, mul #3
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_3, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw () * 3),
	   z0 = svld4 (p0, x0 + svcntw () * 3))

/*
** ld4_s32_4:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s32_4, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw () * 4),
	   z0 = svld4 (p0, x0 + svcntw () * 4))

/*
** ld4_s32_28:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s32_28, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw () * 28),
	   z0 = svld4 (p0, x0 + svcntw () * 28))

/*
** ld4_s32_32:
**	[^{]*
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_s32_32, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 + svcntw () * 32),
	   z0 = svld4 (p0, x0 + svcntw () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_m1:
**	decb	x0
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_m1, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw ()),
	   z0 = svld4 (p0, x0 - svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_m2:
**	decb	x0, all, mul #2
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_m2, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw () * 2),
	   z0 = svld4 (p0, x0 - svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_s32_m3:
**	decb	x0, all, mul #3
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_s32_m3, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw () * 3),
	   z0 = svld4 (p0, x0 - svcntw () * 3))

/*
** ld4_s32_m4:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s32_m4, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw () * 4),
	   z0 = svld4 (p0, x0 - svcntw () * 4))

/*
** ld4_s32_m32:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4_s32_m32, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw () * 32),
	   z0 = svld4 (p0, x0 - svcntw () * 32))

/*
** ld4_s32_m36:
**	[^{]*
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_s32_m36, svint32x4_t, int32_t,
	   z0 = svld4_s32 (p0, x0 - svcntw () * 36),
	   z0 = svld4 (p0, x0 - svcntw () * 36))

/*
** ld4_vnum_s32_0:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_0, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 0),
	   z0 = svld4_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_1:
**	incb	x0
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_1, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 1),
	   z0 = svld4_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_2:
**	incb	x0, all, mul #2
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_2, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 2),
	   z0 = svld4_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_3:
**	incb	x0, all, mul #3
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_3, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 3),
	   z0 = svld4_vnum (p0, x0, 3))

/*
** ld4_vnum_s32_4:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_4, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 4),
	   z0 = svld4_vnum (p0, x0, 4))

/*
** ld4_vnum_s32_28:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_28, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 28),
	   z0 = svld4_vnum (p0, x0, 28))

/*
** ld4_vnum_s32_32:
**	[^{]*
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_32, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, 32),
	   z0 = svld4_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_m1:
**	decb	x0
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m1, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -1),
	   z0 = svld4_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_m2:
**	decb	x0, all, mul #2
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m2, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -2),
	   z0 = svld4_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4_vnum_s32_m3:
**	decb	x0, all, mul #3
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m3, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -3),
	   z0 = svld4_vnum (p0, x0, -3))

/*
** ld4_vnum_s32_m4:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m4, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -4),
	   z0 = svld4_vnum (p0, x0, -4))

/*
** ld4_vnum_s32_m32:
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m32, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -32),
	   z0 = svld4_vnum (p0, x0, -32))

/*
** ld4_vnum_s32_m36:
**	[^{]*
**	ld4w	{z0\.s - z3\.s}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_m36, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, -36),
	   z0 = svld4_vnum (p0, x0, -36))

/* Using MUL to calculate an index would also be OK.  */
/*
** ld4_vnum_s32_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ld4w	{z0\.s - z3\.s}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld4_vnum_s32_x1, svint32x4_t, int32_t,
	   z0 = svld4_vnum_s32 (p0, x0, x1),
	   z0 = svld4_vnum (p0, x0, x1))
