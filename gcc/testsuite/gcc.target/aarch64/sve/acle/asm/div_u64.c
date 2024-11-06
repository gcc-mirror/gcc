/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<63

/*
** div_u64_m_tied1:
**	udiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_m_tied1, svuint64_t,
		z0 = svdiv_u64_m (p0, z0, z1),
		z0 = svdiv_m (p0, z0, z1))

/*
** div_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_u64_m_tied2, svuint64_t,
		z0 = svdiv_u64_m (p0, z1, z0),
		z0 = svdiv_m (p0, z1, z0))

/*
** div_u64_m_untied:
**	movprfx	z0, z1
**	udiv	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_m_untied, svuint64_t,
		z0 = svdiv_u64_m (p0, z1, z2),
		z0 = svdiv_m (p0, z1, z2))

/*
** div_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_m (p0, z0, x0),
		 z0 = svdiv_m (p0, z0, x0))

/*
** div_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_m (p0, z1, x0),
		 z0 = svdiv_m (p0, z1, x0))

/*
** div_1_u64_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_m_tied1, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z0, 1),
		z0 = svdiv_m (p0, z0, 1))

/*
** div_1_u64_m_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_m_untied, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z1, 1),
		z0 = svdiv_m (p0, z1, 1))

/*
** div_2_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_m_tied1, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z0, 2),
		z0 = svdiv_m (p0, z0, 2))

/*
** div_2_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_m_untied, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z1, 2),
		z0 = svdiv_m (p0, z1, 2))

/*
** div_3_u64_m_tied1:
**	mov	(z[0-9]+\.d), #3
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_m_tied1, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z0, 3),
		z0 = svdiv_m (p0, z0, 3))

/*
** div_3_u64_m_untied:
**	mov	(z[0-9]+\.d), #3
**	movprfx	z0, z1
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_m_untied, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z1, 3),
		z0 = svdiv_m (p0, z1, 3))

/*
** div_maxpow_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_m_tied1, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z0, MAXPOW),
		z0 = svdiv_m (p0, z0, MAXPOW))

/*
** div_maxpow_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_m_untied, svuint64_t,
		z0 = svdiv_n_u64_m (p0, z1, MAXPOW),
		z0 = svdiv_m (p0, z1, MAXPOW))

/*
** div_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	udiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_z_tied1, svuint64_t,
		z0 = svdiv_u64_z (p0, z0, z1),
		z0 = svdiv_z (p0, z0, z1))

/*
** div_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_z_tied2, svuint64_t,
		z0 = svdiv_u64_z (p0, z1, z0),
		z0 = svdiv_z (p0, z1, z0))

/*
** div_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	udiv	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_u64_z_untied, svuint64_t,
		z0 = svdiv_u64_z (p0, z1, z2),
		z0 = svdiv_z (p0, z1, z2))

/*
** div_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_z (p0, z0, x0),
		 z0 = svdiv_z (p0, z0, x0))

/*
** div_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	udiv	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_z (p0, z1, x0),
		 z0 = svdiv_z (p0, z1, x0))

/*
** div_1_u64_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.d, p0, z0\.d, z\1.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_z_tied1, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z0, 1),
		z0 = svdiv_z (p0, z0, 1))

/*
** div_1_u64_z_untied:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.d, p0, z1\.d, z\1.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_z_untied, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z1, 1),
		z0 = svdiv_z (p0, z1, 1))

/*
** div_2_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_z_tied1, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z0, 2),
		z0 = svdiv_z (p0, z0, 2))

/*
** div_2_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_z_untied, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z1, 2),
		z0 = svdiv_z (p0, z1, 2))

/*
** div_3_u64_z_tied1:
**	mov	(z[0-9]+\.d), #3
**	movprfx	z0\.d, p0/z, z0\.d
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_z_tied1, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z0, 3),
		z0 = svdiv_z (p0, z0, 3))

/*
** div_3_u64_z_untied:
**	mov	(z[0-9]+\.d), #3
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	udiv	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_z_untied, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z1, 3),
		z0 = svdiv_z (p0, z1, 3))

/*
** div_maxpow_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_z_tied1, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z0, MAXPOW),
		z0 = svdiv_z (p0, z0, MAXPOW))

/*
** div_maxpow_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_z_untied, svuint64_t,
		z0 = svdiv_n_u64_z (p0, z1, MAXPOW),
		z0 = svdiv_z (p0, z1, MAXPOW))

/*
** div_u64_x_tied1:
**	udiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_x_tied1, svuint64_t,
		z0 = svdiv_u64_x (p0, z0, z1),
		z0 = svdiv_x (p0, z0, z1))

/*
** div_u64_x_tied2:
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_u64_x_tied2, svuint64_t,
		z0 = svdiv_u64_x (p0, z1, z0),
		z0 = svdiv_x (p0, z1, z0))

/*
** div_u64_x_untied:
** (
**	movprfx	z0, z1
**	udiv	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_u64_x_untied, svuint64_t,
		z0 = svdiv_u64_x (p0, z1, z2),
		z0 = svdiv_x (p0, z1, z2))

/*
** div_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_x (p0, z0, x0),
		 z0 = svdiv_x (p0, z0, x0))

/*
** div_x0_u64_x_untied:
**	mov	z0\.d, x0
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (div_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svdiv_n_u64_x (p0, z1, x0),
		 z0 = svdiv_x (p0, z1, x0))

/*
** div_1_u64_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_x_tied1, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z0, 1),
		z0 = svdiv_x (p0, z0, 1))

/*
** div_1_u64_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_1_u64_x_untied, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z1, 1),
		z0 = svdiv_x (p0, z1, 1))

/*
** div_2_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_x_tied1, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z0, 2),
		z0 = svdiv_x (p0, z0, 2))

/*
** div_2_u64_x_untied:
**	lsr	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (div_2_u64_x_untied, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z1, 2),
		z0 = svdiv_x (p0, z1, 2))

/*
** div_3_u64_x_tied1:
**	mov	(z[0-9]+\.d), #3
**	udiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_x_tied1, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z0, 3),
		z0 = svdiv_x (p0, z0, 3))

/*
** div_3_u64_x_untied:
**	mov	z0\.d, #3
**	udivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_3_u64_x_untied, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z1, 3),
		z0 = svdiv_x (p0, z1, 3))

/*
** div_maxpow_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_x_tied1, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z0, MAXPOW),
		z0 = svdiv_x (p0, z0, MAXPOW))

/*
** div_maxpow_u64_x_untied:
**	lsr	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (div_maxpow_u64_x_untied, svuint64_t,
		z0 = svdiv_n_u64_x (p0, z1, MAXPOW),
		z0 = svdiv_x (p0, z1, MAXPOW))
