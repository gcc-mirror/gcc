/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** msb_s32_m_tied1:
**	msb	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_m_tied1, svint32_t,
		z0 = svmsb_s32_m (p0, z0, z1, z2),
		z0 = svmsb_m (p0, z0, z1, z2))

/*
** msb_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_m_tied2, svint32_t,
		z0 = svmsb_s32_m (p0, z1, z0, z2),
		z0 = svmsb_m (p0, z1, z0, z2))

/*
** msb_s32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_m_tied3, svint32_t,
		z0 = svmsb_s32_m (p0, z1, z2, z0),
		z0 = svmsb_m (p0, z1, z2, z0))

/*
** msb_s32_m_untied:
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_m_untied, svint32_t,
		z0 = svmsb_s32_m (p0, z1, z2, z3),
		z0 = svmsb_m (p0, z1, z2, z3))

/*
** msb_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svmsb_n_s32_m (p0, z0, z1, x0),
		 z0 = svmsb_m (p0, z0, z1, x0))

/*
** msb_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svmsb_n_s32_m (p0, z1, z2, x0),
		 z0 = svmsb_m (p0, z1, z2, x0))

/*
** msb_11_s32_m_tied1:
**	mov	(z[0-9]+\.s), #11
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_m_tied1, svint32_t,
		z0 = svmsb_n_s32_m (p0, z0, z1, 11),
		z0 = svmsb_m (p0, z0, z1, 11))

/*
** msb_11_s32_m_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_m_untied, svint32_t,
		z0 = svmsb_n_s32_m (p0, z1, z2, 11),
		z0 = svmsb_m (p0, z1, z2, 11))

/*
** msb_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_z_tied1, svint32_t,
		z0 = svmsb_s32_z (p0, z0, z1, z2),
		z0 = svmsb_z (p0, z0, z1, z2))

/*
** msb_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_z_tied2, svint32_t,
		z0 = svmsb_s32_z (p0, z1, z0, z2),
		z0 = svmsb_z (p0, z1, z0, z2))

/*
** msb_s32_z_tied3:
**	movprfx	z0\.s, p0/z, z0\.s
**	mls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_z_tied3, svint32_t,
		z0 = svmsb_s32_z (p0, z1, z2, z0),
		z0 = svmsb_z (p0, z1, z2, z0))

/*
** msb_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	msb	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	msb	z0\.s, p0/m, z1\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z3\.s
**	mls	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (msb_s32_z_untied, svint32_t,
		z0 = svmsb_s32_z (p0, z1, z2, z3),
		z0 = svmsb_z (p0, z1, z2, z3))

/*
** msb_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svmsb_n_s32_z (p0, z0, z1, x0),
		 z0 = svmsb_z (p0, z0, z1, x0))

/*
** msb_w0_s32_z_tied2:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_z_tied2, svint32_t, int32_t,
		 z0 = svmsb_n_s32_z (p0, z1, z0, x0),
		 z0 = svmsb_z (p0, z1, z0, x0))

/*
** msb_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	msb	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	msb	z0\.s, p0/m, z1\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	mls	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svmsb_n_s32_z (p0, z1, z2, x0),
		 z0 = svmsb_z (p0, z1, z2, x0))

/*
** msb_11_s32_z_tied1:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_z_tied1, svint32_t,
		z0 = svmsb_n_s32_z (p0, z0, z1, 11),
		z0 = svmsb_z (p0, z0, z1, 11))

/*
** msb_11_s32_z_tied2:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0\.s, p0/z, z0\.s
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_z_tied2, svint32_t,
		z0 = svmsb_n_s32_z (p0, z1, z0, 11),
		z0 = svmsb_z (p0, z1, z0, 11))

/*
** msb_11_s32_z_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	msb	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	msb	z0\.s, p0/m, z1\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	mls	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_z_untied, svint32_t,
		z0 = svmsb_n_s32_z (p0, z1, z2, 11),
		z0 = svmsb_z (p0, z1, z2, 11))

/*
** msb_s32_x_tied1:
**	msb	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_x_tied1, svint32_t,
		z0 = svmsb_s32_x (p0, z0, z1, z2),
		z0 = svmsb_x (p0, z0, z1, z2))

/*
** msb_s32_x_tied2:
**	msb	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_x_tied2, svint32_t,
		z0 = svmsb_s32_x (p0, z1, z0, z2),
		z0 = svmsb_x (p0, z1, z0, z2))

/*
** msb_s32_x_tied3:
**	mls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_s32_x_tied3, svint32_t,
		z0 = svmsb_s32_x (p0, z1, z2, z0),
		z0 = svmsb_x (p0, z1, z2, z0))

/*
** msb_s32_x_untied:
** (
**	movprfx	z0, z1
**	msb	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0, z2
**	msb	z0\.s, p0/m, z1\.s, z3\.s
** |
**	movprfx	z0, z3
**	mls	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (msb_s32_x_untied, svint32_t,
		z0 = svmsb_s32_x (p0, z1, z2, z3),
		z0 = svmsb_x (p0, z1, z2, z3))

/*
** msb_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svmsb_n_s32_x (p0, z0, z1, x0),
		 z0 = svmsb_x (p0, z0, z1, x0))

/*
** msb_w0_s32_x_tied2:
**	mov	(z[0-9]+\.s), w0
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_x_tied2, svint32_t, int32_t,
		 z0 = svmsb_n_s32_x (p0, z1, z0, x0),
		 z0 = svmsb_x (p0, z1, z0, x0))

/*
** msb_w0_s32_x_untied:
**	mov	z0\.s, w0
**	mls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_ZX (msb_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svmsb_n_s32_x (p0, z1, z2, x0),
		 z0 = svmsb_x (p0, z1, z2, x0))

/*
** msb_11_s32_x_tied1:
**	mov	(z[0-9]+\.s), #11
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_x_tied1, svint32_t,
		z0 = svmsb_n_s32_x (p0, z0, z1, 11),
		z0 = svmsb_x (p0, z0, z1, 11))

/*
** msb_11_s32_x_tied2:
**	mov	(z[0-9]+\.s), #11
**	msb	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_x_tied2, svint32_t,
		z0 = svmsb_n_s32_x (p0, z1, z0, 11),
		z0 = svmsb_x (p0, z1, z0, 11))

/*
** msb_11_s32_x_untied:
**	mov	z0\.s, #11
**	mls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (msb_11_s32_x_untied, svint32_t,
		z0 = svmsb_n_s32_x (p0, z1, z2, 11),
		z0 = svmsb_x (p0, z1, z2, 11))
