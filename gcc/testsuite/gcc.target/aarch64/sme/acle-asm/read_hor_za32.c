/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** read_za32_s32_0_0_tied:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_0_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, 0),
	      z0 = svread_hor_za32_m (z0, p0, 0, 0))

/*
** read_za32_s32_0_1_tied:
**	mov	(w1[2-5]), #?1
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_1_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, 1),
	      z0 = svread_hor_za32_m (z0, p0, 0, 1))

/*
** read_za32_s32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0))

/*
** read_za32_s32_0_w0p1_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\1, 1\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0p1_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, w0 + 1),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0 + 1))

/*
** read_za32_s32_0_w0p3_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0p3_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, w0 + 3),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0 + 3))

/*
** read_za32_s32_0_w0p4_tied:
**	add	(w1[2-5]), w0, #?4
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0p4_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, w0 + 4),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0 + 4))

/*
** read_za32_s32_0_w0m1_tied:
**	sub	(w1[2-5]), w0, #?1
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0m1_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 0, w0 - 1),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0 - 1))

/*
** read_za32_s32_1_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za1h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_1_w0_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 1, w0),
	      z0 = svread_hor_za32_m (z0, p0, 1, w0))

/*
** read_za32_s32_1_w0p3_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za1h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_1_w0p3_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 1, w0 + 3),
	      z0 = svread_hor_za32_m (z0, p0, 1, w0 + 3))

/*
** read_za32_s32_3_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za3h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_3_w0_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 3, w0),
	      z0 = svread_hor_za32_m (z0, p0, 3, w0))

/*
** read_za32_s32_3_w0p3_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za3h\.s\[\1, 3\]
**	ret
*/
TEST_READ_ZA (read_za32_s32_3_w0p3_tied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z0, p0, 3, w0 + 3),
	      z0 = svread_hor_za32_m (z0, p0, 3, w0 + 3))

/*
** read_za32_s32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.s, p0/m, za0h\.s\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za32_s32_0_w0_untied, svint32_t,
	      z0 = svread_hor_za32_s32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za32_m (z1, p0, 0, w0))

/*
** read_za32_u32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_u32_0_w0_tied, svuint32_t,
	      z0 = svread_hor_za32_u32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0))

/*
** read_za32_u32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.s, p0/m, za0h\.s\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za32_u32_0_w0_untied, svuint32_t,
	      z0 = svread_hor_za32_u32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za32_m (z1, p0, 0, w0))

/*
** read_za32_f32_0_w0_tied:
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
**	ret
*/
TEST_READ_ZA (read_za32_f32_0_w0_tied, svfloat32_t,
	      z0 = svread_hor_za32_f32_m (z0, p0, 0, w0),
	      z0 = svread_hor_za32_m (z0, p0, 0, w0))

/*
** read_za32_f32_0_w0_untied:
** (
**	mov	(w1[2-5]), w0
**	mov	z0\.d, z1\.d
**	mova	z0\.s, p0/m, za0h\.s\[\1, 0\]
** |
**	mov	z0\.d, z1\.d
**	mov	(w1[2-5]), w0
**	mova	z0\.s, p0/m, za0h\.s\[\2, 0\]
** |
**	mov	(w1[2-5]), w0
**	mova	z1\.s, p0/m, za0h\.s\[\3, 0\]
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_READ_ZA (read_za32_f32_0_w0_untied, svfloat32_t,
	      z0 = svread_hor_za32_f32_m (z1, p0, 0, w0),
	      z0 = svread_hor_za32_m (z1, p0, 0, w0))
