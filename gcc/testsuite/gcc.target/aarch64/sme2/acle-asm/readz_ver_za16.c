/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za16_s16_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_0, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, 0),
	      z0 = svreadz_ver_za16_s16 (0, 0))

/*
** readz_za16_s16_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_1, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, 1),
	      z0 = svreadz_ver_za16_s16 (0, 1))

/*
** readz_za16_s16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_w0, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, w0),
	      z0 = svreadz_ver_za16_s16 (0, w0))

/*
** readz_za16_s16_0_w0p1:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 1\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_w0p1, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, w0 + 1),
	      z0 = svreadz_ver_za16_s16 (0, w0 + 1))

/*
** readz_za16_s16_0_w0p7:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 7\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_w0p7, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, w0 + 7),
	      z0 = svreadz_ver_za16_s16 (0, w0 + 7))

/*
** readz_za16_s16_0_w0p8:
**	add	(w1[2-5]), w0, #?8
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_w0p8, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, w0 + 8),
	      z0 = svreadz_ver_za16_s16 (0, w0 + 8))

/*
** readz_za16_s16_0_w0m1:
**	sub	(w1[2-5]), w0, #?1
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_0_w0m1, svint16_t,
	      z0 = svreadz_ver_za16_s16 (0, w0 - 1),
	      z0 = svreadz_ver_za16_s16 (0, w0 - 1))

/*
** readz_za16_s16_1_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za1v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_1_w0, svint16_t,
	      z0 = svreadz_ver_za16_s16 (1, w0),
	      z0 = svreadz_ver_za16_s16 (1, w0))

/*
** readz_za16_s16_1_w0p7:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za1v\.h\[\1, 7\]
**	ret
*/
TEST_READ_ZA (readz_za16_s16_1_w0p7, svint16_t,
	      z0 = svreadz_ver_za16_s16 (1, w0 + 7),
	      z0 = svreadz_ver_za16_s16 (1, w0 + 7))

/*
** readz_za16_u16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_u16_0_w0, svuint16_t,
	      z0 = svreadz_ver_za16_u16 (0, w0),
	      z0 = svreadz_ver_za16_u16 (0, w0))

/*
** readz_za16_f16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_f16_0_w0, svfloat16_t,
	      z0 = svreadz_ver_za16_f16 (0, w0),
	      z0 = svreadz_ver_za16_f16 (0, w0))

/*
** readz_za16_bf16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.h, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za16_bf16_0_w0, svbfloat16_t,
	      z0 = svreadz_ver_za16_bf16 (0, w0),
	      z0 = svreadz_ver_za16_bf16 (0, w0))
