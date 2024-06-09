/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** write_za16_s16_0_0_z0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_0_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, 0, p0, z0),
	       svwrite_hor_za16_m (0, 0, p0, z0))

/*
** write_za16_s16_0_1_z0:
**	mov	(w1[2-5]), #?1
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_1_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, 1, p0, z0),
	       svwrite_hor_za16_m (0, 1, p0, z0))

/*
** write_za16_s16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0, p0, z0),
	       svwrite_hor_za16_m (0, w0, p0, z0))

/*
** write_za16_s16_0_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 1\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0p1_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0 + 1, p0, z0),
	       svwrite_hor_za16_m (0, w0 + 1, p0, z0))

/*
** write_za16_s16_0_w0p7_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 7\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0p7_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0 + 7, p0, z0),
	       svwrite_hor_za16_m (0, w0 + 7, p0, z0))

/*
** write_za16_s16_0_w0p8_z0:
**	add	(w1[2-5]), w0, #?8
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0p8_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0 + 8, p0, z0),
	       svwrite_hor_za16_m (0, w0 + 8, p0, z0))

/*
** write_za16_s16_0_w0m1_z0:
**	sub	(w1[2-5]), w0, #?1
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0m1_z0, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0 - 1, p0, z0),
	       svwrite_hor_za16_m (0, w0 - 1, p0, z0))

/*
** write_za16_s16_1_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za1h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_1_w0_z0, svint16_t,
	       svwrite_hor_za16_s16_m (1, w0, p0, z0),
	       svwrite_hor_za16_m (1, w0, p0, z0))

/*
** write_za16_s16_1_w0p7_z0:
**	mov	(w1[2-5]), w0
**	mova	za1h\.h\[\1, 7\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_1_w0p7_z0, svint16_t,
	       svwrite_hor_za16_s16_m (1, w0 + 7, p0, z0),
	       svwrite_hor_za16_m (1, w0 + 7, p0, z0))

/*
** write_za16_s16_0_w0_z1:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 0\], p0/m, z1\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_s16_0_w0_z1, svint16_t,
	       svwrite_hor_za16_s16_m (0, w0, p0, z1),
	       svwrite_hor_za16_m (0, w0, p0, z1))

/*
** write_za16_u16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_u16_0_w0_z0, svuint16_t,
	       svwrite_hor_za16_u16_m (0, w0, p0, z0),
	       svwrite_hor_za16_m (0, w0, p0, z0))

/*
** write_za16_f16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_f16_0_w0_z0, svfloat16_t,
	       svwrite_hor_za16_f16_m (0, w0, p0, z0),
	       svwrite_hor_za16_m (0, w0, p0, z0))

/*
** write_za16_bf16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.h\[\1, 0\], p0/m, z0\.h
**	ret
*/
TEST_WRITE_ZA (write_za16_bf16_0_w0_z0, svbfloat16_t,
	       svwrite_hor_za16_bf16_m (0, w0, p0, z0),
	       svwrite_hor_za16_m (0, w0, p0, z0))
