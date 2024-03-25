/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** read_za8_s8_0_0_tied:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_0_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, 0),
	      z0 = svread_hor_za8_m (z0, p0, 0, 0))

/*
** read_za8_s8_0_1_tied:
**	mov	(w1[2-5]), #?1
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_1_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, 1),
	      z0 = svread_hor_za8_m (z0, p0, 0, 1))

/*
** read_za8_s8_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, w0),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0))

/*
** read_za8_s8_0_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0p1_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, w0 + 1),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0 + 1))

/*
** read_za8_s8_0_w0p15_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\1, 15\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0p15_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, w0 + 15),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0 + 15))

/*
** read_za8_s8_0_w0p16_tied:
**	add	(w1[2-5]), w0, #?16
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0p16_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, w0 + 16),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0 + 16))

/*
** read_za8_s8_0_w0m1_tied:
**	sub	(w1[2-5]), w0, #?1
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0m1_tied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z0, p0, 0, w0 - 1),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0 - 1))

/*
** read_za8_s8_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.b, p0/m, za0h\.b\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za8_s8_0_w0_untied, svint8_t,
	      z0 = svread_hor_za8_s8_m (z1, p0, 0, w0),
	      z0 = svread_hor_za8_m (z1, p0, 0, w0))

/*
** read_za8_u8_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za8_u8_0_w0_tied, svuint8_t,
	      z0 = svread_hor_za8_u8_m (z0, p0, 0, w0),
	      z0 = svread_hor_za8_m (z0, p0, 0, w0))

/*
** read_za8_u8_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.b, p0/m, za0h\.b\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.b, p0/m, za0h\.b\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.b, p0/m, za0h\.b\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za8_u8_0_w0_untied, svuint8_t,
	      z0 = svread_hor_za8_u8_m (z1, p0, 0, w0),
	      z0 = svread_hor_za8_m (z1, p0, 0, w0))
