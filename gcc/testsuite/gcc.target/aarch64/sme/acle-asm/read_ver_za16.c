/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** read_za16_s16_0_0_tied:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_0_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, 0),
	      z0 = svread_ver_za16_m (z0, p0, 0, 0))

/*
** read_za16_s16_0_1_tied:
**	mov	(w1[2-5]), #?1
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_1_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, 1),
	      z0 = svread_ver_za16_m (z0, p0, 0, 1))

/*
** read_za16_s16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, w0),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0))

/*
** read_za16_s16_0_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0p1_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, w0 + 1),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0 + 1))

/*
** read_za16_s16_0_w0p7_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 7\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0p7_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, w0 + 7),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0 + 7))

/*
** read_za16_s16_0_w0p8_tied:
**	add	(w1[2-5]), w0, #?8
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0p8_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, w0 + 8),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0 + 8))

/*
** read_za16_s16_0_w0m1_tied:
**	sub	(w1[2-5]), w0, #?1
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0m1_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 0, w0 - 1),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0 - 1))

/*
** read_za16_s16_1_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za1v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_1_w0_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 1, w0),
	      z0 = svread_ver_za16_m (z0, p0, 1, w0))

/*
** read_za16_s16_1_w0p7_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za1v\.h\[\1, 7\]
**	ret
*/
TEST_READ_ZA (read_za16_s16_1_w0p7_tied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z0, p0, 1, w0 + 7),
	      z0 = svread_ver_za16_m (z0, p0, 1, w0 + 7))

/*
** read_za16_s16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.h, p0/m, za0v\.h\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za16_s16_0_w0_untied, svint16_t,
	      z0 = svread_ver_za16_s16_m (z1, p0, 0, w0),
	      z0 = svread_ver_za16_m (z1, p0, 0, w0))

/*
** read_za16_u16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_u16_0_w0_tied, svuint16_t,
	      z0 = svread_ver_za16_u16_m (z0, p0, 0, w0),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0))

/*
** read_za16_u16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.h, p0/m, za0v\.h\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za16_u16_0_w0_untied, svuint16_t,
	      z0 = svread_ver_za16_u16_m (z1, p0, 0, w0),
	      z0 = svread_ver_za16_m (z1, p0, 0, w0))

/*
** read_za16_f16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_f16_0_w0_tied, svfloat16_t,
	      z0 = svread_ver_za16_f16_m (z0, p0, 0, w0),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0))

/*
** read_za16_f16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.h, p0/m, za0v\.h\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za16_f16_0_w0_untied, svfloat16_t,
	      z0 = svread_ver_za16_f16_m (z1, p0, 0, w0),
	      z0 = svread_ver_za16_m (z1, p0, 0, w0))

/*
** read_za16_bf16_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za16_bf16_0_w0_tied, svbfloat16_t,
	      z0 = svread_ver_za16_bf16_m (z0, p0, 0, w0),
	      z0 = svread_ver_za16_m (z0, p0, 0, w0))

/*
** read_za16_bf16_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.h, p0/m, za0v\.h\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.h, p0/m, za0v\.h\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.h, p0/m, za0v\.h\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za16_bf16_0_w0_untied, svbfloat16_t,
	      z0 = svread_ver_za16_bf16_m (z1, p0, 0, w0),
	      z0 = svread_ver_za16_m (z1, p0, 0, w0))
