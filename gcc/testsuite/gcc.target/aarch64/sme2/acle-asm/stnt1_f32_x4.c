/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sme2_acle.h"

/*
** stnt1_f32_base:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_base, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0, z0),
		  svstnt1 (pn8, x0, z0))

/*
** stnt1_f32_index:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_index, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + x1, z0),
		  svstnt1 (pn8, x0 + x1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_1:
**	incb	x0
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_1, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw (), z0),
		  svstnt1 (pn8, x0 + svcntw (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_2:
**	incb	x0, all, mul #2
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_2, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw () * 2, z0),
		  svstnt1 (pn8, x0 + svcntw () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_3:
**	incb	x0, all, mul #3
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_3, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw () * 3, z0),
		  svstnt1 (pn8, x0 + svcntw () * 3, z0))

/*
** stnt1_f32_4:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_4, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw () * 4, z0),
		  svstnt1 (pn8, x0 + svcntw () * 4, z0))

/*
** stnt1_f32_28:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_28, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw () * 28, z0),
		  svstnt1 (pn8, x0 + svcntw () * 28, z0))

/*
** stnt1_f32_32:
**	[^{]*
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_32, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 + svcntw () * 32, z0),
		  svstnt1 (pn8, x0 + svcntw () * 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_m1:
**	decb	x0
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m1, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw (), z0),
		  svstnt1 (pn8, x0 - svcntw (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_m2:
**	decb	x0, all, mul #2
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m2, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw () * 2, z0),
		  svstnt1 (pn8, x0 - svcntw () * 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_f32_m3:
**	decb	x0, all, mul #3
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m3, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw () * 3, z0),
		  svstnt1 (pn8, x0 - svcntw () * 3, z0))

/*
** stnt1_f32_m4:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m4, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw () * 4, z0),
		  svstnt1 (pn8, x0 - svcntw () * 4, z0))

/*
** stnt1_f32_m32:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m32, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw () * 32, z0),
		  svstnt1 (pn8, x0 - svcntw () * 32, z0))

/*
** stnt1_f32_m36:
**	[^{]*
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_m36, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0 - svcntw () * 36, z0),
		  svstnt1 (pn8, x0 - svcntw () * 36, z0))

/*
** stnt1_f32_z17:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1w	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_z17, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0, z17),
		  svstnt1 (pn8, x0, z17))

/*
** stnt1_f32_z22:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	stnt1w	{z[^\n]+}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_z22, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0, z22),
		  svstnt1 (pn8, x0, z22))

/*
** stnt1_f32_z28:
**	stnt1w	{z28\.s - z31\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_z28, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn8, x0, z28),
		  svstnt1 (pn8, x0, z28))

/*
** stnt1_f32_pn0:
**	mov	p([89]|1[0-5])\.b, p0\.b
**	stnt1w	{z0\.s - z3\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_pn0, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn0, x0, z0),
		  svstnt1 (pn0, x0, z0))

/*
** stnt1_f32_pn7:
**	mov	p([89]|1[0-5])\.b, p7\.b
**	stnt1w	{z0\.s - z3\.s}, pn\1, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_pn7, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn7, x0, z0),
		  svstnt1 (pn7, x0, z0))

/*
** stnt1_f32_pn15:
**	stnt1w	{z0\.s - z3\.s}, pn15, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_f32_pn15, svfloat32x4_t, float32_t,
		  svstnt1_f32_x4 (pn15, x0, z0),
		  svstnt1 (pn15, x0, z0))

/*
** stnt1_vnum_f32_0:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_0, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 0, z0),
		  svstnt1_vnum (pn8, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_1:
**	incb	x0
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_1, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 1, z0),
		  svstnt1_vnum (pn8, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_2:
**	incb	x0, all, mul #2
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_2, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 2, z0),
		  svstnt1_vnum (pn8, x0, 2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_3:
**	incb	x0, all, mul #3
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_3, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 3, z0),
		  svstnt1_vnum (pn8, x0, 3, z0))

/*
** stnt1_vnum_f32_4:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_4, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 4, z0),
		  svstnt1_vnum (pn8, x0, 4, z0))

/*
** stnt1_vnum_f32_28:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #28, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_28, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 28, z0),
		  svstnt1_vnum (pn8, x0, 28, z0))

/*
** stnt1_vnum_f32_32:
**	[^{]*
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_32, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, 32, z0),
		  svstnt1_vnum (pn8, x0, 32, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_m1:
**	decb	x0
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m1, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -1, z0),
		  svstnt1_vnum (pn8, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_m2:
**	decb	x0, all, mul #2
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m2, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -2, z0),
		  svstnt1_vnum (pn8, x0, -2, z0))

/* Moving the constant into a register would also be OK.  */
/*
** stnt1_vnum_f32_m3:
**	decb	x0, all, mul #3
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m3, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -3, z0),
		  svstnt1_vnum (pn8, x0, -3, z0))

/*
** stnt1_vnum_f32_m4:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #-4, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m4, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -4, z0),
		  svstnt1_vnum (pn8, x0, -4, z0))

/*
** stnt1_vnum_f32_m32:
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, #-32, mul vl\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m32, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -32, z0),
		  svstnt1_vnum (pn8, x0, -32, z0))

/*
** stnt1_vnum_f32_m36:
**	[^{]*
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x[0-9]+\]
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_m36, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, -36, z0),
		  svstnt1_vnum (pn8, x0, -36, z0))

/*
** stnt1_vnum_f32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	stnt1w	{z0\.s - z3\.s}, pn8, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	stnt1w	{z0\.s - z3\.s}, pn8, \[x0, \3\]
** )
**	ret
*/
TEST_STORE_COUNT (stnt1_vnum_f32_x1, svfloat32x4_t, float32_t,
		  svstnt1_vnum_f32_x4 (pn8, x0, x1, z0),
		  svstnt1_vnum (pn8, x0, x1, z0))
