/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** write_za128_s8_0_0_z0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_0_z0, svint8_t,
	       svwrite_ver_za128_s8_m (0, 0, p0, z0),
	       svwrite_ver_za128_m (0, 0, p0, z0))

/*
** write_za128_s8_0_1_z0:
**	mov	(w1[2-5]), #?1
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_1_z0, svint8_t,
	       svwrite_ver_za128_s8_m (0, 1, p0, z0),
	       svwrite_ver_za128_m (0, 1, p0, z0))

/*
** write_za128_s8_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_w0_z0, svint8_t,
	       svwrite_ver_za128_s8_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_s8_0_w0p1_z0:
**	add	(w1[2-5]), w0, #?1
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_w0p1_z0, svint8_t,
	       svwrite_ver_za128_s8_m (0, w0 + 1, p0, z0),
	       svwrite_ver_za128_m (0, w0 + 1, p0, z0))

/*
** write_za128_s8_0_w0m1_z0:
**	sub	(w1[2-5]), w0, #?1
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_w0m1_z0, svint8_t,
	       svwrite_ver_za128_s8_m (0, w0 - 1, p0, z0),
	       svwrite_ver_za128_m (0, w0 - 1, p0, z0))

/*
** write_za128_s8_1_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za1v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_1_w0_z0, svint8_t,
	       svwrite_ver_za128_s8_m (1, w0, p0, z0),
	       svwrite_ver_za128_m (1, w0, p0, z0))

/*
** write_za128_s8_15_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za15v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_15_w0_z0, svint8_t,
	       svwrite_ver_za128_s8_m (15, w0, p0, z0),
	       svwrite_ver_za128_m (15, w0, p0, z0))

/*
** write_za128_s8_0_w0_z1:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z1\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s8_0_w0_z1, svint8_t,
	       svwrite_ver_za128_s8_m (0, w0, p0, z1),
	       svwrite_ver_za128_m (0, w0, p0, z1))

/*
** write_za128_u8_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_u8_0_w0_z0, svuint8_t,
	       svwrite_ver_za128_u8_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_s16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s16_0_w0_z0, svint16_t,
	       svwrite_ver_za128_s16_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_u16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_u16_0_w0_z0, svuint16_t,
	       svwrite_ver_za128_u16_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_f16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_f16_0_w0_z0, svfloat16_t,
	       svwrite_ver_za128_f16_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_bf16_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_bf16_0_w0_z0, svbfloat16_t,
	       svwrite_ver_za128_bf16_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_s32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s32_0_w0_z0, svint32_t,
	       svwrite_ver_za128_s32_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_u32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_u32_0_w0_z0, svuint32_t,
	       svwrite_ver_za128_u32_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_f32_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_f32_0_w0_z0, svfloat32_t,
	       svwrite_ver_za128_f32_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_s64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_s64_0_w0_z0, svint64_t,
	       svwrite_ver_za128_s64_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_u64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_u64_0_w0_z0, svuint64_t,
	       svwrite_ver_za128_u64_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))

/*
** write_za128_f64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.q\[\1, 0\], p0/m, z0\.q
**	ret
*/
TEST_WRITE_ZA (write_za128_f64_0_w0_z0, svfloat64_t,
	       svwrite_ver_za128_f64_m (0, w0, p0, z0),
	       svwrite_ver_za128_m (0, w0, p0, z0))
