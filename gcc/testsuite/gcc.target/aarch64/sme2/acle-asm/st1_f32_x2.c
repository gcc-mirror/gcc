/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** st1_f32_base:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_base, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0, z0),
		  svst1 (pn8, x0, z0))

/*
** st1_f32_index:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_index, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 + x1, z0),
		  svst1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_f32_1:
**	incb	x0
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_1, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 + svcntw (), z0),
		  svst1 (pn8, x0 + svcntw (), z0))

/*
** st1_f32_2:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_2, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 + svcntw () * 2, z0),
		  svst1 (pn8, x0 + svcntw () * 2, z0))

/*
** st1_f32_14:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_14, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 + svcntw () * 14, z0),
		  svst1 (pn8, x0 + svcntw () * 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_f32_16:
**	incb	x0, all, mul #16
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_16, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 + svcntw () * 16, z0),
		  svst1 (pn8, x0 + svcntw () * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_f32_m1:
**	decb	x0
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_m1, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 - svcntw (), z0),
		  svst1 (pn8, x0 - svcntw (), z0))

/*
** st1_f32_m2:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_m2, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 - svcntw () * 2, z0),
		  svst1 (pn8, x0 - svcntw () * 2, z0))

/*
** st1_f32_m16:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_m16, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 - svcntw () * 16, z0),
		  svst1 (pn8, x0 - svcntw () * 16, z0))

/*
** st1_f32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_m18, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0 - svcntw () * 18, z0),
		  svst1 (pn8, x0 - svcntw () * 18, z0))

/*
** st1_f32_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	st1w	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_z17, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0, z17),
		  svst1 (pn8, x0, z17))

/*
** st1_f32_z22:
**	st1w	{z22\.s(?: - |, )z23\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_z22, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0, z22),
		  svst1 (pn8, x0, z22))

/*
** st1_f32_z28:
**	st1w	{z28\.s(?: - |, )z29\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_z28, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn8, x0, z28),
		  svst1 (pn8, x0, z28))

/*
** st1_f32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	st1w	{z0\.s(?: - |, )z1\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_pn0, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn0, x0, z0),
		  svst1 (pn0, x0, z0))

/*
** st1_f32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	st1w	{z0\.s(?: - |, )z1\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_pn7, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn7, x0, z0),
		  svst1 (pn7, x0, z0))

/*
** st1_f32_pn15:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_f32_pn15, svfloat32x2_t, float32_t,
		  svst1_f32_x2 (pn15, x0, z0),
		  svst1 (pn15, x0, z0))

/*
** st1_vnum_f32_0:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_0, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, 0, z0),
		  svst1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_f32_1:
**	incb	x0
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_1, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, 1, z0),
		  svst1_vnum (pn8, x0, 1, z0))

/*
** st1_vnum_f32_2:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_2, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, 2, z0),
		  svst1_vnum (pn8, x0, 2, z0))

/*
** st1_vnum_f32_14:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #14, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_14, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, 14, z0),
		  svst1_vnum (pn8, x0, 14, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_f32_16:
**	incb	x0, all, mul #16
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_16, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, 16, z0),
		  svst1_vnum (pn8, x0, 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st1_vnum_f32_m1:
**	decb	x0
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_m1, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, -1, z0),
		  svst1_vnum (pn8, x0, -1, z0))

/*
** st1_vnum_f32_m2:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-2, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_m2, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, -2, z0),
		  svst1_vnum (pn8, x0, -2, z0))

/*
** st1_vnum_f32_m16:
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, #-16, mul vl\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_m16, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, -16, z0),
		  svst1_vnum (pn8, x0, -16, z0))

/*
** st1_vnum_f32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\1\]
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_m18, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, -18, z0),
		  svst1_vnum (pn8, x0, -18, z0))

/*
** st1_vnum_f32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st1w	{z0\.s(?: - |, )z1\.s}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (st1_vnum_f32_x1, svfloat32x2_t, float32_t,
		  svst1_vnum_f32_x2 (pn8, x0, x1, z0),
		  svst1_vnum (pn8, x0, x1, z0))
