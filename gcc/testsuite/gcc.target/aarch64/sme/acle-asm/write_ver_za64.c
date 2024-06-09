/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** write_za64_s64_0_0_z0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_0_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, 0, p0, z0),
	       svwrite_ver_za64_m (0, 0, p0, z0))

/*
** write_za64_s64_0_1_z0:
**	mov	(w1[2-5]), #?1
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_1_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, 1, p0, z0),
	       svwrite_ver_za64_m (0, 1, p0, z0))

/*
** write_za64_s64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_w0_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, w0, p0, z0),
	       svwrite_ver_za64_m (0, w0, p0, z0))

/*
** write_za64_s64_0_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.d\[\1, 1\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_w0p1_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, w0 + 1, p0, z0),
	       svwrite_ver_za64_m (0, w0 + 1, p0, z0))

/*
** write_za64_s64_0_w0p2_z0:
**	add	(w1[2-5]), w0, #?2
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_w0p2_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, w0 + 2, p0, z0),
	       svwrite_ver_za64_m (0, w0 + 2, p0, z0))

/*
** write_za64_s64_0_w0m1_z0:
**	sub	(w1[2-5]), w0, #?1
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_w0m1_z0, svint64_t,
	       svwrite_ver_za64_s64_m (0, w0 - 1, p0, z0),
	       svwrite_ver_za64_m (0, w0 - 1, p0, z0))

/*
** write_za64_s64_1_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za1v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_1_w0_z0, svint64_t,
	       svwrite_ver_za64_s64_m (1, w0, p0, z0),
	       svwrite_ver_za64_m (1, w0, p0, z0))

/*
** write_za64_s64_1_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za1v\.d\[\1, 1\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_1_w0p1_z0, svint64_t,
	       svwrite_ver_za64_s64_m (1, w0 + 1, p0, z0),
	       svwrite_ver_za64_m (1, w0 + 1, p0, z0))

/*
** write_za64_s64_7_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za7v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_7_w0_z0, svint64_t,
	       svwrite_ver_za64_s64_m (7, w0, p0, z0),
	       svwrite_ver_za64_m (7, w0, p0, z0))

/*
** write_za64_s64_7_w0p1_z0:
**	mov	(w1[2-5]), w0
**	mova	za7v\.d\[\1, 1\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_7_w0p1_z0, svint64_t,
	       svwrite_ver_za64_s64_m (7, w0 + 1, p0, z0),
	       svwrite_ver_za64_m (7, w0 + 1, p0, z0))

/*
** write_za64_s64_0_w0_z1:
**	mov	(w1[2-5]), w0
**	mova	za0v\.d\[\1, 0\], p0/m, z1\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_s64_0_w0_z1, svint64_t,
	       svwrite_ver_za64_s64_m (0, w0, p0, z1),
	       svwrite_ver_za64_m (0, w0, p0, z1))

/*
** write_za64_u64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_u64_0_w0_z0, svuint64_t,
	       svwrite_ver_za64_u64_m (0, w0, p0, z0),
	       svwrite_ver_za64_m (0, w0, p0, z0))

/*
** write_za64_f64_0_w0_z0:
**	mov	(w1[2-5]), w0
**	mova	za0v\.d\[\1, 0\], p0/m, z0\.d
**	ret
*/
TEST_WRITE_ZA (write_za64_f64_0_w0_z0, svfloat64_t,
	       svwrite_ver_za64_f64_m (0, w0, p0, z0),
	       svwrite_ver_za64_m (0, w0, p0, z0))
