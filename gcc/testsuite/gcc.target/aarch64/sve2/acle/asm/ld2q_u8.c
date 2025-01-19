/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** ld2q_u8_base:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_u8_base, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0),
	   z0 = svld2q (p0, x0))

/*
** ld2q_u8_index:
**	add	(x[0-9]), (?:x0, x1|x1, x0)
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_u8_index, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + x1),
	   z0 = svld2q (p0, x0 + x1))

/*
** ld2q_u8_index2:
**	add	(x[0-9]), x0, x1, lsl #?1
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_u8_index2, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + x1 * 2),
	   z0 = svld2q (p0, x0 + x1 * 2))

/*
** ld2q_u8_index4:
**	add	(x[0-9]), x0, x1, lsl #?2
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_u8_index4, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + x1 * 4),
	   z0 = svld2q (p0, x0 + x1 * 4))

/*
** ld2q_u8_index8:
**	add	(x[0-9]), x0, x1, lsl #?3
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_u8_index8, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + x1 * 8),
	   z0 = svld2q (p0, x0 + x1 * 8))

/*
** ld2q_u8_index16:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_LOAD (ld2q_u8_index16, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + x1 * 16),
	   z0 = svld2q (p0, x0 + x1 * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_u8_1:
**	incb	x0
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_u8_1, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + svcntb ()),
	   z0 = svld2q (p0, x0 + svcntb ()))

/*
** ld2q_u8_2:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_u8_2, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + svcntb () * 2),
	   z0 = svld2q (p0, x0 + svcntb () * 2))

/*
** ld2q_u8_14:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_u8_14, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + svcntb () * 14),
	   z0 = svld2q (p0, x0 + svcntb () * 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_u8_16:
**	incb	x0, all, mul #16
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_u8_16, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 + svcntb () * 16),
	   z0 = svld2q (p0, x0 + svcntb () * 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_u8_m1:
**	decb	x0
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_u8_m1, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 - svcntb ()),
	   z0 = svld2q (p0, x0 - svcntb ()))

/*
** ld2q_u8_m2:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_u8_m2, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 - svcntb () * 2),
	   z0 = svld2q (p0, x0 - svcntb () * 2))

/*
** ld2q_u8_m16:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_u8_m16, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 - svcntb () * 16),
	   z0 = svld2q (p0, x0 - svcntb () * 16))

/*
** ld2q_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_u8_m18, svuint8x2_t, uint8_t,
	   z0 = svld2q_u8 (p0, x0 - svcntb () * 18),
	   z0 = svld2q (p0, x0 - svcntb () * 18))

/*
** ld2q_vnum_u8_0:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_0, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, 0),
	   z0 = svld2q_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_vnum_u8_1:
**	incb	x0
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_1, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, 1),
	   z0 = svld2q_vnum (p0, x0, 1))

/*
** ld2q_vnum_u8_2:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #2, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_2, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, 2),
	   z0 = svld2q_vnum (p0, x0, 2))

/*
** ld2q_vnum_u8_14:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #14, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_14, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, 14),
	   z0 = svld2q_vnum (p0, x0, 14))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_vnum_u8_16:
**	incb	x0, all, mul #16
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_16, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, 16),
	   z0 = svld2q_vnum (p0, x0, 16))

/* Moving the constant into a register would also be OK.  */
/*
** ld2q_vnum_u8_m1:
**	decb	x0
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_m1, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, -1),
	   z0 = svld2q_vnum (p0, x0, -1))

/*
** ld2q_vnum_u8_m2:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #-2, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_m2, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, -2),
	   z0 = svld2q_vnum (p0, x0, -2))

/*
** ld2q_vnum_u8_m16:
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[x0, #-16, mul vl\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_m16, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, -16),
	   z0 = svld2q_vnum (p0, x0, -16))

/*
** ld2q_vnum_u8_m18:
**	addvl	(x[0-9]+), x0, #-18
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_m18, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, -18),
	   z0 = svld2q_vnum (p0, x0, -18))

/*
** ld2q_vnum_u8_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	ld2q	{z0\.q(?: - |, )z1\.q}, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ld2q_vnum_u8_x1, svuint8x2_t, uint8_t,
	   z0 = svld2q_vnum_u8 (p0, x0, x1),
	   z0 = svld2q_vnum (p0, x0, x1))
