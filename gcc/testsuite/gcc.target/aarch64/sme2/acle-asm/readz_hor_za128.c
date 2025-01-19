/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** readz_za128_s8_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_0_0, svint8_t,
	      z0 = svreadz_hor_za128_s8 (0, 0),
	      z0 = svreadz_hor_za128_s8 (0, 0))

/*
** readz_za128_s8_0_1:
**	mov	(w1[2-5]), #?1
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_0_1, svint8_t,
	      z0 = svreadz_hor_za128_s8 (0, 1),
	      z0 = svreadz_hor_za128_s8 (0, 1))

/*
** readz_za128_s8_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_0_w0, svint8_t,
	      z0 = svreadz_hor_za128_s8 (0, w0),
	      z0 = svreadz_hor_za128_s8 (0, w0))

/*
** readz_za128_s8_0_w0p1:
**	add	(w1[2-5]), w0, #?1
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_0_w0p1, svint8_t,
	      z0 = svreadz_hor_za128_s8 (0, w0 + 1),
	      z0 = svreadz_hor_za128_s8 (0, w0 + 1))

/*
** readz_za128_s8_0_w0m1:
**	sub	(w1[2-5]), w0, #?1
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_0_w0m1, svint8_t,
	      z0 = svreadz_hor_za128_s8 (0, w0 - 1),
	      z0 = svreadz_hor_za128_s8 (0, w0 - 1))

/*
** readz_za128_s8_1_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za1h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_1_w0, svint8_t,
	      z0 = svreadz_hor_za128_s8 (1, w0),
	      z0 = svreadz_hor_za128_s8 (1, w0))

/*
** readz_za128_s8_15_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za15h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s8_15_w0, svint8_t,
	      z0 = svreadz_hor_za128_s8 (15, w0),
	      z0 = svreadz_hor_za128_s8 (15, w0))

/*
** readz_za128_u8_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_u8_0_w0, svuint8_t,
	      z0 = svreadz_hor_za128_u8 (0, w0),
	      z0 = svreadz_hor_za128_u8 (0, w0))

/*
** readz_za128_s16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s16_0_w0, svint16_t,
	      z0 = svreadz_hor_za128_s16 (0, w0),
	      z0 = svreadz_hor_za128_s16 (0, w0))

/*
** readz_za128_u16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_u16_0_w0, svuint16_t,
	      z0 = svreadz_hor_za128_u16 (0, w0),
	      z0 = svreadz_hor_za128_u16 (0, w0))

/*
** readz_za128_f16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_f16_0_w0, svfloat16_t,
	      z0 = svreadz_hor_za128_f16 (0, w0),
	      z0 = svreadz_hor_za128_f16 (0, w0))

/*
** readz_za128_bf16_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_bf16_0_w0, svbfloat16_t,
	      z0 = svreadz_hor_za128_bf16 (0, w0),
	      z0 = svreadz_hor_za128_bf16 (0, w0))

/*
** readz_za128_s32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s32_0_w0, svint32_t,
	      z0 = svreadz_hor_za128_s32 (0, w0),
	      z0 = svreadz_hor_za128_s32 (0, w0))

/*
** readz_za128_u32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_u32_0_w0, svuint32_t,
	      z0 = svreadz_hor_za128_u32 (0, w0),
	      z0 = svreadz_hor_za128_u32 (0, w0))

/*
** readz_za128_f32_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_f32_0_w0, svfloat32_t,
	      z0 = svreadz_hor_za128_f32 (0, w0),
	      z0 = svreadz_hor_za128_f32 (0, w0))

/*
** readz_za128_s64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_s64_0_w0, svint64_t,
	      z0 = svreadz_hor_za128_s64 (0, w0),
	      z0 = svreadz_hor_za128_s64 (0, w0))

/*
** readz_za128_u64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_u64_0_w0, svuint64_t,
	      z0 = svreadz_hor_za128_u64 (0, w0),
	      z0 = svreadz_hor_za128_u64 (0, w0))

/*
** readz_za128_f64_0_w0:
**	mov	(w1[2-5]), w0
**	movaz	z0\.q, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (readz_za128_f64_0_w0, svfloat64_t,
	      z0 = svreadz_hor_za128_f64 (0, w0),
	      z0 = svreadz_hor_za128_f64 (0, w0))
