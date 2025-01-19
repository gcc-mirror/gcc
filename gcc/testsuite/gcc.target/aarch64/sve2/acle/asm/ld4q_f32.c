/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld4q_f32_base:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_base, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0),
	   z0 = svld4q (p0, x0))

/*
** ld4q_f32_index:
**	add	(x[0-9]), x0, x1, lsl #?2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_f32_index, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + x1),
	   z0 = svld4q (p0, x0 + x1))

/*
** ld4q_f32_index2:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld4q_f32_index2, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + x1 * 2),
	   z0 = svld4q (p0, x0 + x1 * 2))

/*
** ld4q_f32_index4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld4q_f32_index4, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + x1 * 4),
	   z0 = svld4q (p0, x0 + x1 * 4))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_1, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw ()),
	   z0 = svld4q (p0, x0 + svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_2, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw () * 2),
	   z0 = svld4q (p0, x0 + svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_3, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw () * 3),
	   z0 = svld4q (p0, x0 + svcntw () * 3))

/*
** ld4q_f32_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_f32_4, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw () * 4),
	   z0 = svld4q (p0, x0 + svcntw () * 4))

/*
** ld4q_f32_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_f32_28, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw () * 28),
	   z0 = svld4q (p0, x0 + svcntw () * 28))

/*
** ld4q_f32_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_f32_32, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 + svcntw () * 32),
	   z0 = svld4q (p0, x0 + svcntw () * 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_m1, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw ()),
	   z0 = svld4q (p0, x0 - svcntw ()))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_m2, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw () * 2),
	   z0 = svld4q (p0, x0 - svcntw () * 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_f32_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_f32_m3, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw () * 3),
	   z0 = svld4q (p0, x0 - svcntw () * 3))

/*
** ld4q_f32_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_f32_m4, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw () * 4),
	   z0 = svld4q (p0, x0 - svcntw () * 4))

/*
** ld4q_f32_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_f32_m32, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw () * 32),
	   z0 = svld4q (p0, x0 - svcntw () * 32))

/*
** ld4q_f32_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_f32_m36, svfloat32x4_t, float32_t,
	   z0 = svld4q_f32 (p0, x0 - svcntw () * 36),
	   z0 = svld4q (p0, x0 - svcntw () * 36))

/*
** ld4q_vnum_f32_0:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_0, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 0),
	   z0 = svld4q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_1:
**	incb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_1, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 1),
	   z0 = svld4q_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_2:
**	incb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_2, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 2),
	   z0 = svld4q_vnum (p0, x0, 2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_3:
**	incb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_3, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 3),
	   z0 = svld4q_vnum (p0, x0, 3))

/*
** ld4q_vnum_f32_4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_4, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 4),
	   z0 = svld4q_vnum (p0, x0, 4))

/*
** ld4q_vnum_f32_28:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #28, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_28, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 28),
	   z0 = svld4q_vnum (p0, x0, 28))

/*
** ld4q_vnum_f32_32:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_32, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, 32),
	   z0 = svld4q_vnum (p0, x0, 32))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_m1:
**	decb	x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m1, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -1),
	   z0 = svld4q_vnum (p0, x0, -1))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_m2:
**	decb	x0, all, mul #2
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m2, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -2),
	   z0 = svld4q_vnum (p0, x0, -2))

/* Moving the constant into a register would also be OK.  */
/*
** ld4q_vnum_f32_m3:
**	decb	x0, all, mul #3
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m3, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -3),
	   z0 = svld4q_vnum (p0, x0, -3))

/*
** ld4q_vnum_f32_m4:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-4, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m4, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -4),
	   z0 = svld4q_vnum (p0, x0, -4))

/*
** ld4q_vnum_f32_m32:
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, #-32, mul vl\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m32, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -32),
	   z0 = svld4q_vnum (p0, x0, -32))

/*
** ld4q_vnum_f32_m36:
**	[^{]*
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x[0-9]+\]
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_m36, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, -36),
	   z0 = svld4q_vnum (p0, x0, -36))

/*
** ld4q_vnum_f32_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld4q	{z0\.q - z3\.q}, p0/z, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	ld4q	{z0\.q - z3\.q}, p0/z, \[x0, \3\]
** )
**	ret
*/
TEST_LOAD (ld4q_vnum_f32_x1, svfloat32x4_t, float32_t,
	   z0 = svld4q_vnum_f32 (p0, x0, x1),
	   z0 = svld4q_vnum (p0, x0, x1))
