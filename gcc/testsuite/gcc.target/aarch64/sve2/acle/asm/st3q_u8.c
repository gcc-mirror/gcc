/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"

/*
** st3q_u8_base:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u8_base, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0, z0),
	    svst3q (p0, x0, z0))

/*
** st3q_u8_index:
**	add	(x[0-9]), (?:x0, x1|x1, x0)
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_index, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + x1, z0),
	    svst3q (p0, x0 + x1, z0))

/*
** st3q_u8_index2:
**	add	(x[0-9]), x0, x1, lsl #?1
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_index2, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + x1 * 2, z0),
	    svst3q (p0, x0 + x1 * 2, z0))

/*
** st3q_u8_index4:
**	add	(x[0-9]), x0, x1, lsl #?2
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_index4, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + x1 * 4, z0),
	    svst3q (p0, x0 + x1 * 4, z0))

/*
** st3q_u8_index8:
**	add	(x[0-9]), x0, x1, lsl #?3
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_index8, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + x1 * 8, z0),
	    svst3q (p0, x0 + x1 * 8, z0))

/*
** st3q_u8_index16:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, x1, lsl #?4\]
**	ret
*/
TEST_STORE (st3q_u8_index16, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + x1 * 16, z0),
	    svst3q (p0, x0 + x1 * 16, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u8_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u8_1, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + svcntb (), z0),
	    svst3q (p0, x0 + svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u8_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u8_2, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + svcntb () * 2, z0),
	    svst3q (p0, x0 + svcntb () * 2, z0))

/*
** st3q_u8_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_u8_3, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + svcntb () * 3, z0),
	    svst3q (p0, x0 + svcntb () * 3, z0))

/*
** st3q_u8_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_u8_21, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + svcntb () * 21, z0),
	    svst3q (p0, x0 + svcntb () * 21, z0))

/*
** st3q_u8_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_24, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 + svcntb () * 24, z0),
	    svst3q (p0, x0 + svcntb () * 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u8_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u8_m1, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 - svcntb (), z0),
	    svst3q (p0, x0 - svcntb (), z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_u8_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_u8_m2, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 - svcntb () * 2, z0),
	    svst3q (p0, x0 - svcntb () * 2, z0))

/*
** st3q_u8_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_u8_m3, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 - svcntb () * 3, z0),
	    svst3q (p0, x0 - svcntb () * 3, z0))

/*
** st3q_u8_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_u8_m24, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 - svcntb () * 24, z0),
	    svst3q (p0, x0 - svcntb () * 24, z0))

/*
** st3q_u8_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_u8_m27, svuint8x3_t, uint8_t,
	    svst3q_u8 (p0, x0 - svcntb () * 27, z0),
	    svst3q (p0, x0 - svcntb () * 27, z0))

/*
** st3q_vnum_u8_0:
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_0, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 0, z0),
	    svst3q_vnum (p0, x0, 0, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u8_1:
**	incb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_1, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 1, z0),
	    svst3q_vnum (p0, x0, 1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u8_2:
**	incb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_2, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 2, z0),
	    svst3q_vnum (p0, x0, 2, z0))

/*
** st3q_vnum_u8_3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_3, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 3, z0),
	    svst3q_vnum (p0, x0, 3, z0))

/*
** st3q_vnum_u8_21:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #21, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_21, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 21, z0),
	    svst3q_vnum (p0, x0, 21, z0))

/*
** st3q_vnum_u8_24:
**	addvl	(x[0-9]+), x0, #24
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_24, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, 24, z0),
	    svst3q_vnum (p0, x0, 24, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u8_m1:
**	decb	x0
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_m1, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, -1, z0),
	    svst3q_vnum (p0, x0, -1, z0))

/* Moving the constant into a register would also be OK.  */
/*
** st3q_vnum_u8_m2:
**	decb	x0, all, mul #2
**	st3q	{z0\.q - z2\.q}, p0, \[x0\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_m2, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, -2, z0),
	    svst3q_vnum (p0, x0, -2, z0))

/*
** st3q_vnum_u8_m3:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-3, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_m3, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, -3, z0),
	    svst3q_vnum (p0, x0, -3, z0))

/*
** st3q_vnum_u8_m24:
**	st3q	{z0\.q - z2\.q}, p0, \[x0, #-24, mul vl\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_m24, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, -24, z0),
	    svst3q_vnum (p0, x0, -24, z0))

/*
** st3q_vnum_u8_m27:
**	addvl	(x[0-9]+), x0, #-27
**	st3q	{z0\.q - z2\.q}, p0, \[\1\]
**	ret
*/
TEST_STORE (st3q_vnum_u8_m27, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, -27, z0),
	    svst3q_vnum (p0, x0, -27, z0))

/*
** st3q_vnum_u8_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	st3q	{z0\.q - z2\.q}, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	st3q	{z0\.q - z2\.q}, p0, \[x0, \3\]
** )
**	ret
*/
TEST_STORE (st3q_vnum_u8_x1, svuint8x3_t, uint8_t,
	    svst3q_vnum_u8 (p0, x0, x1, z0),
	    svst3q_vnum (p0, x0, x1, z0))
