/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** read_za64_s64_0_0_tied:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_0_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, 0),
	      z0 = svread_hor_za64_m (z0, p0, 0, 0))

/*
** read_za64_s64_0_1_tied:
**	mov	(w1[2-5]), #?1
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_1_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, 1),
	      z0 = svread_hor_za64_m (z0, p0, 0, 1))

/*
** read_za64_s64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_w0_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0))

/*
** read_za64_s64_0_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_w0p1_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, w0 + 1),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0 + 1))

/*
** read_za64_s64_0_w0p2_tied:
**	add	(w1[2-5]), w0, #?2
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_w0p2_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, w0 + 2),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0 + 2))

/*
** read_za64_s64_0_w0m1_tied:
**	sub	(w1[2-5]), w0, #?1
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_w0m1_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 0, w0 - 1),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0 - 1))

/*
** read_za64_s64_1_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za1h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_1_w0_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 1, w0),
	      z0 = svread_hor_za64_m (z0, p0, 1, w0))

/*
** read_za64_s64_1_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za1h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_1_w0p1_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 1, w0 + 1),
	      z0 = svread_hor_za64_m (z0, p0, 1, w0 + 1))

/*
** read_za64_s64_7_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za7h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_7_w0_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 7, w0),
	      z0 = svread_hor_za64_m (z0, p0, 7, w0))

/*
** read_za64_s64_7_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za7h\.d\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za64_s64_7_w0p1_tied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z0, p0, 7, w0 + 1),
	      z0 = svread_hor_za64_m (z0, p0, 7, w0 + 1))

/*
** read_za64_s64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.d, p0/m, za0h\.d\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za64_s64_0_w0_untied, svint64_t,
	      z0 = svread_hor_za64_s64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za64_m (z1, p0, 0, w0))

/*
** read_za64_u64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_u64_0_w0_tied, svuint64_t,
	      z0 = svread_hor_za64_u64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0))

/*
** read_za64_u64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.d, p0/m, za0h\.d\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za64_u64_0_w0_untied, svuint64_t,
	      z0 = svread_hor_za64_u64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za64_m (z1, p0, 0, w0))

/*
** read_za64_f64_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za64_f64_0_w0_tied, svfloat64_t,
	      z0 = svread_hor_za64_f64_m (z0, p0, 0, w0),
	      z0 = svread_hor_za64_m (z0, p0, 0, w0))

/*
** read_za64_f64_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.d, p0/m, za0h\.d\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.d, p0/m, za0h\.d\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.d, p0/m, za0h\.d\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za64_f64_0_w0_untied, svfloat64_t,
	      z0 = svread_hor_za64_f64_m (z1, p0, 0, w0),
	      z0 = svread_hor_za64_m (z1, p0, 0, w0))
