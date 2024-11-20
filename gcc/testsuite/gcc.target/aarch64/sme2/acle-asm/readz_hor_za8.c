/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za8_s8_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_0, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, 0),
	      z0 = svreadz_hor_za8_s8 (0, 0))

/*
** readz_za8_s8_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_1, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, 1),
	      z0 = svreadz_hor_za8_s8 (0, 1))

/*
** readz_za8_s8_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_w0, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, w0),
	      z0 = svreadz_hor_za8_s8 (0, w0))

/*
** readz_za8_s8_0_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.b, za0h\.b\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_w0p1, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, w0 + 1),
	      z0 = svreadz_hor_za8_s8 (0, w0 + 1))

/*
** readz_za8_s8_0_w0p15:
**	mov	(w1[2-5]), w0
**	movaz	z0\.b, za0h\.b\[\1, 15\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_w0p15, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, w0 + 15),
	      z0 = svreadz_hor_za8_s8 (0, w0 + 15))

/*
** readz_za8_s8_0_w0p16:
**	add	(w1[2-5]), w0, #?16
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_w0p16, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, w0 + 16),
	      z0 = svreadz_hor_za8_s8 (0, w0 + 16))

/*
** readz_za8_s8_0_w0m1:
**	sub	(w1[2-5]), w0, #?1
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_s8_0_w0m1, svint8_t,
	      z0 = svreadz_hor_za8_s8 (0, w0 - 1),
	      z0 = svreadz_hor_za8_s8 (0, w0 - 1))

/*
** readz_za8_u8_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.b, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za8_u8_0_w0, svuint8_t,
	      z0 = svreadz_hor_za8_u8 (0, w0),
	      z0 = svreadz_hor_za8_u8 (0, w0))
