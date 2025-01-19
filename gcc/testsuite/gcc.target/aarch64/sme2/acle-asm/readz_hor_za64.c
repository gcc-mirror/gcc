/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za64_s64_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_0, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, 0),
	      z0 = svreadz_hor_za64_s64 (0, 0))

/*
** readz_za64_s64_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_1, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, 1),
	      z0 = svreadz_hor_za64_s64 (0, 1))

/*
** readz_za64_s64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_w0, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, w0),
	      z0 = svreadz_hor_za64_s64 (0, w0))

/*
** readz_za64_s64_0_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za0h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_w0p1, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, w0 + 1),
	      z0 = svreadz_hor_za64_s64 (0, w0 + 1))

/*
** readz_za64_s64_0_w0p2:
**	add	(w1[2-5]), w0, #?2
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_w0p2, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, w0 + 2),
	      z0 = svreadz_hor_za64_s64 (0, w0 + 2))

/*
** readz_za64_s64_0_w0m1:
**	sub	(w1[2-5]), w0, #?1
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_0_w0m1, svint64_t,
	      z0 = svreadz_hor_za64_s64 (0, w0 - 1),
	      z0 = svreadz_hor_za64_s64 (0, w0 - 1))

/*
** readz_za64_s64_1_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za1h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_1_w0, svint64_t,
	      z0 = svreadz_hor_za64_s64 (1, w0),
	      z0 = svreadz_hor_za64_s64 (1, w0))

/*
** readz_za64_s64_1_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za1h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_1_w0p1, svint64_t,
	      z0 = svreadz_hor_za64_s64 (1, w0 + 1),
	      z0 = svreadz_hor_za64_s64 (1, w0 + 1))

/*
** readz_za64_s64_7_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za7h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_7_w0, svint64_t,
	      z0 = svreadz_hor_za64_s64 (7, w0),
	      z0 = svreadz_hor_za64_s64 (7, w0))

/*
** readz_za64_s64_7_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za7h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za64_s64_7_w0p1, svint64_t,
	      z0 = svreadz_hor_za64_s64 (7, w0 + 1),
	      z0 = svreadz_hor_za64_s64 (7, w0 + 1))

/*
** readz_za64_u64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_u64_0_w0, svuint64_t,
	      z0 = svreadz_hor_za64_u64 (0, w0),
	      z0 = svreadz_hor_za64_u64 (0, w0))

/*
** readz_za64_f64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.d, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za64_f64_0_w0, svfloat64_t,
	      z0 = svreadz_hor_za64_f64 (0, w0),
	      z0 = svreadz_hor_za64_f64 (0, w0))
