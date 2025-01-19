/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za32_s32_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_0, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, 0),
	      z0 = svreadz_hor_za32_s32 (0, 0))

/*
** readz_za32_s32_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_1, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, 1),
	      z0 = svreadz_hor_za32_s32 (0, 1))

/*
** readz_za32_s32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_w0, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, w0),
	      z0 = svreadz_hor_za32_s32 (0, w0))

/*
** readz_za32_s32_0_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za0h\.s\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_w0p1, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, w0 + 1),
	      z0 = svreadz_hor_za32_s32 (0, w0 + 1))

/*
** readz_za32_s32_0_w0p3:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za0h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_w0p3, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, w0 + 3),
	      z0 = svreadz_hor_za32_s32 (0, w0 + 3))

/*
** readz_za32_s32_0_w0p4:
**	add	(w1[2-5]), w0, #?4
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_w0p4, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, w0 + 4),
	      z0 = svreadz_hor_za32_s32 (0, w0 + 4))

/*
** readz_za32_s32_0_w0m1:
**	sub	(w1[2-5]), w0, #?1
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_0_w0m1, svint32_t,
	      z0 = svreadz_hor_za32_s32 (0, w0 - 1),
	      z0 = svreadz_hor_za32_s32 (0, w0 - 1))

/*
** readz_za32_s32_1_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za1h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_1_w0, svint32_t,
	      z0 = svreadz_hor_za32_s32 (1, w0),
	      z0 = svreadz_hor_za32_s32 (1, w0))

/*
** readz_za32_s32_1_w0p3:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za1h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_1_w0p3, svint32_t,
	      z0 = svreadz_hor_za32_s32 (1, w0 + 3),
	      z0 = svreadz_hor_za32_s32 (1, w0 + 3))

/*
** readz_za32_s32_3_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za3h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_3_w0, svint32_t,
	      z0 = svreadz_hor_za32_s32 (3, w0),
	      z0 = svreadz_hor_za32_s32 (3, w0))

/*
** readz_za32_s32_3_w0p3:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za3h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (readz_za32_s32_3_w0p3, svint32_t,
	      z0 = svreadz_hor_za32_s32 (3, w0 + 3),
	      z0 = svreadz_hor_za32_s32 (3, w0 + 3))

/*
** readz_za32_u32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_u32_0_w0, svuint32_t,
	      z0 = svreadz_hor_za32_u32 (0, w0),
	      z0 = svreadz_hor_za32_u32 (0, w0))

/*
** readz_za32_f32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.s, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za32_f32_0_w0, svfloat32_t,
	      z0 = svreadz_hor_za32_f32 (0, w0),
	      z0 = svreadz_hor_za32_f32 (0, w0))
