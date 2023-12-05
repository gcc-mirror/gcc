/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** write_za32_s32_0_0_z0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_0_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, 0, p0, z0),
	       svwrite_hor_za32_m (0, 0, p0, z0))

/*
** write_za32_s32_0_1_z0:
**	mov	(w1[2-5]), #?1
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_1_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, 1, p0, z0),
	       svwrite_hor_za32_m (0, 1, p0, z0))

/*
** write_za32_s32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0, p0, z0),
	       svwrite_hor_za32_m (0, w0, p0, z0))

/*
** write_za32_s32_0_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 1\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0p1_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0 + 1, p0, z0),
	       svwrite_hor_za32_m (0, w0 + 1, p0, z0))

/*
** write_za32_s32_0_w0p3_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 3\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0p3_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0 + 3, p0, z0),
	       svwrite_hor_za32_m (0, w0 + 3, p0, z0))

/*
** write_za32_s32_0_w0p4_z0:
**	add	(w1[2-5]), w0, #?4
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0p4_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0 + 4, p0, z0),
	       svwrite_hor_za32_m (0, w0 + 4, p0, z0))

/*
** write_za32_s32_0_w0m1_z0:
**	sub	(w1[2-5]), w0, #?1
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0m1_z0, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0 - 1, p0, z0),
	       svwrite_hor_za32_m (0, w0 - 1, p0, z0))

/*
** write_za32_s32_1_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za1h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_1_w0_z0, svint32_t,
	       svwrite_hor_za32_s32_m (1, w0, p0, z0),
	       svwrite_hor_za32_m (1, w0, p0, z0))

/*
** write_za32_s32_1_w0p3_z0:
**	mov	(w1[2-5]), w0
**	mova	za1h\.s\[\1, 3\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_1_w0p3_z0, svint32_t,
	       svwrite_hor_za32_s32_m (1, w0 + 3, p0, z0),
	       svwrite_hor_za32_m (1, w0 + 3, p0, z0))

/*
** write_za32_s32_3_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za3h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_3_w0_z0, svint32_t,
	       svwrite_hor_za32_s32_m (3, w0, p0, z0),
	       svwrite_hor_za32_m (3, w0, p0, z0))

/*
** write_za32_s32_3_w0p3_z0:
**	mov	(w1[2-5]), w0
**	mova	za3h\.s\[\1, 3\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_3_w0p3_z0, svint32_t,
	       svwrite_hor_za32_s32_m (3, w0 + 3, p0, z0),
	       svwrite_hor_za32_m (3, w0 + 3, p0, z0))

/*
** write_za32_s32_0_w0_z1:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 0\], p0/m, z1\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_s32_0_w0_z1, svint32_t,
	       svwrite_hor_za32_s32_m (0, w0, p0, z1),
	       svwrite_hor_za32_m (0, w0, p0, z1))

/*
** write_za32_u32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_u32_0_w0_z0, svuint32_t,
	       svwrite_hor_za32_u32_m (0, w0, p0, z0),
	       svwrite_hor_za32_m (0, w0, p0, z0))

/*
** write_za32_f32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0h\.s\[\1, 0\], p0/m, z0\.s
**	ret
*/
TEST_WRITE_ZA (write_za32_f32_0_w0_z0, svfloat32_t,
	       svwrite_hor_za32_f32_m (0, w0, p0, z0),
	       svwrite_hor_za32_m (0, w0, p0, z0))
