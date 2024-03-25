/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** write_za8_s8_0_0_z0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_0_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, 0, p0, z0),
	       svwrite_ver_za8_m (0, 0, p0, z0))

/*
** write_za8_s8_0_1_z0:
**	mov	(w1[2-5]), #?1
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_1_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, 1, p0, z0),
	       svwrite_ver_za8_m (0, 1, p0, z0))

/*
** write_za8_s8_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0, p0, z0),
	       svwrite_ver_za8_m (0, w0, p0, z0))

/*
** write_za8_s8_0_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.b\[\1, 1\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0p1_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0 + 1, p0, z0),
	       svwrite_ver_za8_m (0, w0 + 1, p0, z0))

/*
** write_za8_s8_0_w0p15_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.b\[\1, 15\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0p15_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0 + 15, p0, z0),
	       svwrite_ver_za8_m (0, w0 + 15, p0, z0))

/*
** write_za8_s8_0_w0p16_z0:
**	add	(w1[2-5]), w0, #?16
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0p16_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0 + 16, p0, z0),
	       svwrite_ver_za8_m (0, w0 + 16, p0, z0))

/*
** write_za8_s8_0_w0m1_z0:
**	sub	(w1[2-5]), w0, #?1
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0m1_z0, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0 - 1, p0, z0),
	       svwrite_ver_za8_m (0, w0 - 1, p0, z0))

/*
** write_za8_s8_0_w0_z1:
**	mov	(w1[2-5]), w0
**	mova	za0v\.b\[\1, 0\], p0/m, z1\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_s8_0_w0_z1, svint8_t,
	       svwrite_ver_za8_s8_m (0, w0, p0, z1),
	       svwrite_ver_za8_m (0, w0, p0, z1))

/*
** write_za8_u8_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.b\[\1, 0\], p0/m, z0\.b
**	ret
*/
TEST_WRITE_ZA (write_za8_u8_0_w0_z0, svuint8_t,
	       svwrite_ver_za8_u8_m (0, w0, p0, z0),
	       svwrite_ver_za8_m (0, w0, p0, z0))
