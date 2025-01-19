/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** ldnt1_f32_base:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_base, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0),
		 z0 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f32_index:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_index, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 + x1),
		 z0 = svldnt1_x2 (pn8, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f32_1:
**	incb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_1, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 + svcntw ()),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw ()))

/*
** ldnt1_f32_2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_2, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 + svcntw () * 2),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 2))

/*
** ldnt1_f32_14:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_14, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 + svcntw () * 14),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f32_16:
**	incb	x0, all, mul #16
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_16, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 + svcntw () * 16),
		 z0 = svldnt1_x2 (pn8, x0 + svcntw () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_f32_m1:
**	decb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_m1, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 - svcntw ()),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw ()))

/*
** ldnt1_f32_m2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_m2, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 - svcntw () * 2),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 2))

/*
** ldnt1_f32_m16:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_m16, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 - svcntw () * 16),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 16))

/*
** ldnt1_f32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_m18, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn8, x0 - svcntw () * 18),
		 z0 = svldnt1_x2 (pn8, x0 - svcntw () * 18))

/*
** ldnt1_f32_z17:
**	ldnt1w	{z[^\n]+}, pn8/z, \[x0\]
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_z17, svfloat32x2_t, float32_t,
		 z17 = svldnt1_f32_x2 (pn8, x0),
		 z17 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f32_z22:
**	ldnt1w	{z22\.s(?: - |, )z23\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_z22, svfloat32x2_t, float32_t,
		 z22 = svldnt1_f32_x2 (pn8, x0),
		 z22 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f32_z28:
**	ldnt1w	{z28\.s(?: - |, )z29\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_z28, svfloat32x2_t, float32_t,
		 z28 = svldnt1_f32_x2 (pn8, x0),
		 z28 = svldnt1_x2 (pn8, x0))

/*
** ldnt1_f32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_pn0, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn0, x0),
		 z0 = svldnt1_x2 (pn0, x0))

/*
** ldnt1_f32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn\1/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_pn7, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn7, x0),
		 z0 = svldnt1_x2 (pn7, x0))

/*
** ldnt1_f32_pn15:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn15/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_f32_pn15, svfloat32x2_t, float32_t,
		 z0 = svldnt1_f32_x2 (pn15, x0),
		 z0 = svldnt1_x2 (pn15, x0))

/*
** ldnt1_vnum_f32_0:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_0, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, 0),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f32_1:
**	incb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_1, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, 1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 1))

/*
** ldnt1_vnum_f32_2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_2, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, 2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 2))

/*
** ldnt1_vnum_f32_14:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_14, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, 14),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f32_16:
**	incb	x0, all, mul #16
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_16, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, 16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ldnt1_vnum_f32_m1:
**	decb	x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_m1, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, -1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -1))

/*
** ldnt1_vnum_f32_m2:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_m2, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, -2),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -2))

/*
** ldnt1_vnum_f32_m16:
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_m16, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, -16),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -16))

/*
** ldnt1_vnum_f32_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\1\]
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_m18, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, -18),
		 z0 = svldnt1_vnum_x2 (pn8, x0, -18))

/*
** ldnt1_vnum_f32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ldnt1w	{z0\.s(?: - |, )z1\.s}, pn8/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD_COUNT (ldnt1_vnum_f32_x1, svfloat32x2_t, float32_t,
		 z0 = svldnt1_vnum_f32_x2 (pn8, x0, x1),
		 z0 = svldnt1_vnum_x2 (pn8, x0, x1))
