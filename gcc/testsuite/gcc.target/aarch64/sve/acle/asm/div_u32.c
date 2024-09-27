/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1<<31

/*
** div_u32_m_tied1:
**	udiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_m_tied1, svuint32_t,
		z0 = svdiv_u32_m (p0, z0, z1),
		z0 = svdiv_m (p0, z0, z1))

/*
** div_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	udiv	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_m_tied2, svuint32_t,
		z0 = svdiv_u32_m (p0, z1, z0),
		z0 = svdiv_m (p0, z1, z0))

/*
** div_u32_m_untied:
**	movprfx	z0, z1
**	udiv	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_m_untied, svuint32_t,
		z0 = svdiv_u32_m (p0, z1, z2),
		z0 = svdiv_m (p0, z1, z2))

/*
** div_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_m (p0, z0, x0),
		 z0 = svdiv_m (p0, z0, x0))

/*
** div_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_m (p0, z1, x0),
		 z0 = svdiv_m (p0, z1, x0))

/*
** div_1_u32_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_m_tied1, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z0, 1),
		z0 = svdiv_m (p0, z0, 1))

/*
** div_1_u32_m_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_m_untied, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z1, 1),
		z0 = svdiv_m (p0, z1, 1))

/*
** div_2_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_m_tied1, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z0, 2),
		z0 = svdiv_m (p0, z0, 2))

/*
** div_2_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_m_untied, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z1, 2),
		z0 = svdiv_m (p0, z1, 2))

/*
** div_3_u32_m_tied1:
**	mov	(z[0-9]+\.s), #3
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_m_tied1, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z0, 3),
		z0 = svdiv_m (p0, z0, 3))

/*
** div_3_u32_m_untied:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0, z1
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_m_untied, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z1, 3),
		z0 = svdiv_m (p0, z1, 3))

/*
** div_maxpow_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_m_tied1, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z0, MAXPOW),
		z0 = svdiv_m (p0, z0, MAXPOW))

/*
** div_maxpow_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_m_untied, svuint32_t,
		z0 = svdiv_n_u32_m (p0, z1, MAXPOW),
		z0 = svdiv_m (p0, z1, MAXPOW))

/*
** div_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	udiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_z_tied1, svuint32_t,
		z0 = svdiv_u32_z (p0, z0, z1),
		z0 = svdiv_z (p0, z0, z1))

/*
** div_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_z_tied2, svuint32_t,
		z0 = svdiv_u32_z (p0, z1, z0),
		z0 = svdiv_z (p0, z1, z0))

/*
** div_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	udiv	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_u32_z_untied, svuint32_t,
		z0 = svdiv_u32_z (p0, z1, z2),
		z0 = svdiv_z (p0, z1, z2))

/*
** div_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_z (p0, z0, x0),
		 z0 = svdiv_z (p0, z0, x0))

/*
** div_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	udiv	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_z (p0, z1, x0),
		 z0 = svdiv_z (p0, z1, x0))

/*
** div_1_u32_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.s, p0, z0\.s, z\1.s
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_z_tied1, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z0, 1),
		z0 = svdiv_z (p0, z0, 1))

/*
** div_1_u32_z_untied:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.s, p0, z1\.s, z\1.s
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_z_untied, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z1, 1),
		z0 = svdiv_z (p0, z1, 1))

/*
** div_2_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_z_tied1, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z0, 2),
		z0 = svdiv_z (p0, z0, 2))

/*
** div_2_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_z_untied, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z1, 2),
		z0 = svdiv_z (p0, z1, 2))

/*
** div_3_u32_z_tied1:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0\.s, p0/z, z0\.s
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_z_tied1, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z0, 3),
		z0 = svdiv_z (p0, z0, 3))

/*
** div_3_u32_z_untied:
**	mov	(z[0-9]+\.s), #3
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	udiv	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_z_untied, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z1, 3),
		z0 = svdiv_z (p0, z1, 3))

/*
** div_maxpow_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_z_tied1, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z0, MAXPOW),
		z0 = svdiv_z (p0, z0, MAXPOW))

/*
** div_maxpow_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_z_untied, svuint32_t,
		z0 = svdiv_n_u32_z (p0, z1, MAXPOW),
		z0 = svdiv_z (p0, z1, MAXPOW))

/*
** div_u32_x_tied1:
**	udiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_x_tied1, svuint32_t,
		z0 = svdiv_u32_x (p0, z0, z1),
		z0 = svdiv_x (p0, z0, z1))

/*
** div_u32_x_tied2:
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_u32_x_tied2, svuint32_t,
		z0 = svdiv_u32_x (p0, z1, z0),
		z0 = svdiv_x (p0, z1, z0))

/*
** div_u32_x_untied:
** (
**	movprfx	z0, z1
**	udiv	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_u32_x_untied, svuint32_t,
		z0 = svdiv_u32_x (p0, z1, z2),
		z0 = svdiv_x (p0, z1, z2))

/*
** div_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_x (p0, z0, x0),
		 z0 = svdiv_x (p0, z0, x0))

/*
** div_w0_u32_x_untied:
**	mov	z0\.s, w0
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (div_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svdiv_n_u32_x (p0, z1, x0),
		 z0 = svdiv_x (p0, z1, x0))

/*
** div_1_u32_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_x_tied1, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z0, 1),
		z0 = svdiv_x (p0, z0, 1))

/*
** div_1_u32_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u32_x_untied, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z1, 1),
		z0 = svdiv_x (p0, z1, 1))

/*
** div_2_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_x_tied1, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z0, 2),
		z0 = svdiv_x (p0, z0, 2))

/*
** div_2_u32_x_untied:
**	lsr	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u32_x_untied, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z1, 2),
		z0 = svdiv_x (p0, z1, 2))

/*
** div_3_u32_x_tied1:
**	mov	(z[0-9]+\.s), #3
**	udiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_x_tied1, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z0, 3),
		z0 = svdiv_x (p0, z0, 3))

/*
** div_3_u32_x_untied:
**	mov	z0\.s, #3
**	udivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_3_u32_x_untied, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z1, 3),
		z0 = svdiv_x (p0, z1, 3))

/*
** div_maxpow_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_x_tied1, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z0, MAXPOW),
		z0 = svdiv_x (p0, z0, MAXPOW))

/*
** div_maxpow_u32_x_untied:
**	lsr	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u32_x_untied, svuint32_t,
		z0 = svdiv_n_u32_x (p0, z1, MAXPOW),
		z0 = svdiv_x (p0, z1, MAXPOW))
