/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** read_za128_s8_0_0_tied:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_0_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 0, 0),
	      z0 = svread_hor_za128_m (z0, p0, 0, 0))

/*
** read_za128_s8_0_1_tied:
**	mov	(w1[2-5]), #?1
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_1_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 0, 1),
	      z0 = svread_hor_za128_m (z0, p0, 0, 1))

/*
** read_za128_s8_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_w0_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_s8_0_w0p1_tied:
**	add	(w1[2-5]), w0, #?1
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_w0p1_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 0, w0 + 1),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0 + 1))

/*
** read_za128_s8_0_w0m1_tied:
**	sub	(w1[2-5]), w0, #?1
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_w0m1_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 0, w0 - 1),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0 - 1))

/*
** read_za128_s8_1_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za1h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_1_w0_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 1, w0),
	      z0 = svread_hor_za128_m (z0, p0, 1, w0))

/*
** read_za128_s8_15_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za15h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s8_15_w0_tied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z0, p0, 15, w0),
	      z0 = svread_hor_za128_m (z0, p0, 15, w0))

/*
** read_za128_s8_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_s8_0_w0_untied, svint8_t,
	      z0 = svread_hor_za128_s8_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_u8_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_u8_0_w0_tied, svuint8_t,
	      z0 = svread_hor_za128_u8_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_u8_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_u8_0_w0_untied, svuint8_t,
	      z0 = svread_hor_za128_u8_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_s16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s16_0_w0_tied, svint16_t,
	      z0 = svread_hor_za128_s16_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_s16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_s16_0_w0_untied, svint16_t,
	      z0 = svread_hor_za128_s16_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_u16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_u16_0_w0_tied, svuint16_t,
	      z0 = svread_hor_za128_u16_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_u16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_u16_0_w0_untied, svuint16_t,
	      z0 = svread_hor_za128_u16_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_f16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_f16_0_w0_tied, svfloat16_t,
	      z0 = svread_hor_za128_f16_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_f16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_f16_0_w0_untied, svfloat16_t,
	      z0 = svread_hor_za128_f16_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_bf16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_bf16_0_w0_tied, svbfloat16_t,
	      z0 = svread_hor_za128_bf16_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_bf16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_bf16_0_w0_untied, svbfloat16_t,
	      z0 = svread_hor_za128_bf16_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_s32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s32_0_w0_tied, svint32_t,
	      z0 = svread_hor_za128_s32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_s32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_s32_0_w0_untied, svint32_t,
	      z0 = svread_hor_za128_s32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_u32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_u32_0_w0_tied, svuint32_t,
	      z0 = svread_hor_za128_u32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_u32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_u32_0_w0_untied, svuint32_t,
	      z0 = svread_hor_za128_u32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_f32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_f32_0_w0_tied, svfloat32_t,
	      z0 = svread_hor_za128_f32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_f32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_f32_0_w0_untied, svfloat32_t,
	      z0 = svread_hor_za128_f32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_s64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_s64_0_w0_tied, svint64_t,
	      z0 = svread_hor_za128_s64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_s64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_s64_0_w0_untied, svint64_t,
	      z0 = svread_hor_za128_s64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_u64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_u64_0_w0_tied, svuint64_t,
	      z0 = svread_hor_za128_u64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_u64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_u64_0_w0_untied, svuint64_t,
	      z0 = svread_hor_za128_u64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))

/*
** read_za128_f64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za128_f64_0_w0_tied, svfloat64_t,
	      z0 = svread_hor_za128_f64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za128_m (z0, p0, 0, w0))

/*
** read_za128_f64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.q, p0/m, za0h\.q\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.q, p0/m, za0h\.q\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.q, p0/m, za0h\.q\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za128_f64_0_w0_untied, svfloat64_t,
	      z0 = svread_hor_za128_f64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za128_m (z1, p0, 0, w0))
