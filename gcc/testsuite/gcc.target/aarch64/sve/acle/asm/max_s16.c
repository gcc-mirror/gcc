/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** max_s16_m_tied1:
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_m_tied1, svint16_t,
		z0 = svmax_s16_m (p0, z0, z1),
		z0 = svmax_m (p0, z0, z1))

/*
** max_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smax	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_m_tied2, svint16_t,
		z0 = svmax_s16_m (p0, z1, z0),
		z0 = svmax_m (p0, z1, z0))

/*
** max_s16_m_untied:
**	movprfx	z0, z1
**	smax	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_m_untied, svint16_t,
		z0 = svmax_s16_m (p0, z1, z2),
		z0 = svmax_m (p0, z1, z2))

/*
** max_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svmax_n_s16_m (p0, z0, x0),
		 z0 = svmax_m (p0, z0, x0))

/*
** max_w0_s16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svmax_n_s16_m (p0, z1, x0),
		 z0 = svmax_m (p0, z1, x0))

/*
** max_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_m_tied1, svint16_t,
		z0 = svmax_n_s16_m (p0, z0, 1),
		z0 = svmax_m (p0, z0, 1))

/*
** max_1_s16_m_untied:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_m_untied, svint16_t,
		z0 = svmax_n_s16_m (p0, z1, 1),
		z0 = svmax_m (p0, z1, 1))

/*
** max_m1_s16_m:
**	mov	(z[0-9]+)\.b, #-1
**	smax	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (max_m1_s16_m, svint16_t,
		z0 = svmax_n_s16_m (p0, z0, -1),
		z0 = svmax_m (p0, z0, -1))

/*
** max_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_z_tied1, svint16_t,
		z0 = svmax_s16_z (p0, z0, z1),
		z0 = svmax_z (p0, z0, z1))

/*
** max_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_z_tied2, svint16_t,
		z0 = svmax_s16_z (p0, z1, z0),
		z0 = svmax_z (p0, z1, z0))

/*
** max_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	smax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	smax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_s16_z_untied, svint16_t,
		z0 = svmax_s16_z (p0, z1, z2),
		z0 = svmax_z (p0, z1, z2))

/*
** max_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svmax_n_s16_z (p0, z0, x0),
		 z0 = svmax_z (p0, z0, x0))

/*
** max_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	smax	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	smax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svmax_n_s16_z (p0, z1, x0),
		 z0 = svmax_z (p0, z1, x0))

/*
** max_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_z_tied1, svint16_t,
		z0 = svmax_n_s16_z (p0, z0, 1),
		z0 = svmax_z (p0, z0, 1))

/*
** max_1_s16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	smax	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	smax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_z_untied, svint16_t,
		z0 = svmax_n_s16_z (p0, z1, 1),
		z0 = svmax_z (p0, z1, 1))

/*
** max_s16_x_tied1:
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_x_tied1, svint16_t,
		z0 = svmax_s16_x (p0, z0, z1),
		z0 = svmax_x (p0, z0, z1))

/*
** max_s16_x_tied2:
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (max_s16_x_tied2, svint16_t,
		z0 = svmax_s16_x (p0, z1, z0),
		z0 = svmax_x (p0, z1, z0))

/*
** max_s16_x_untied:
** (
**	movprfx	z0, z1
**	smax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	smax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (max_s16_x_untied, svint16_t,
		z0 = svmax_s16_x (p0, z1, z2),
		z0 = svmax_x (p0, z1, z2))

/*
** max_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svmax_n_s16_x (p0, z0, x0),
		 z0 = svmax_x (p0, z0, x0))

/*
** max_w0_s16_x_untied:
**	mov	z0\.h, w0
**	smax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (max_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svmax_n_s16_x (p0, z1, x0),
		 z0 = svmax_x (p0, z1, x0))

/*
** max_1_s16_x_tied1:
**	smax	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_x_tied1, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, 1),
		z0 = svmax_x (p0, z0, 1))

/*
** max_1_s16_x_untied:
**	movprfx	z0, z1
**	smax	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (max_1_s16_x_untied, svint16_t,
		z0 = svmax_n_s16_x (p0, z1, 1),
		z0 = svmax_x (p0, z1, 1))

/*
** max_127_s16_x:
**	smax	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (max_127_s16_x, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, 127),
		z0 = svmax_x (p0, z0, 127))

/*
** max_128_s16_x:
**	mov	(z[0-9]+\.h), #128
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_128_s16_x, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, 128),
		z0 = svmax_x (p0, z0, 128))

/*
** max_m1_s16_x:
**	smax	z0\.h, z0\.h, #-1
**	ret
*/
TEST_UNIFORM_Z (max_m1_s16_x, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, -1),
		z0 = svmax_x (p0, z0, -1))

/*
** max_m128_s16_x:
**	smax	z0\.h, z0\.h, #-128
**	ret
*/
TEST_UNIFORM_Z (max_m128_s16_x, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, -128),
		z0 = svmax_x (p0, z0, -128))

/*
** max_m129_s16_x:
**	mov	(z[0-9]+\.h), #-129
**	smax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (max_m129_s16_x, svint16_t,
		z0 = svmax_n_s16_x (p0, z0, -129),
		z0 = svmax_x (p0, z0, -129))
