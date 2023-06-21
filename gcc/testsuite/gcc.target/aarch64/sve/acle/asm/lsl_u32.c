/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_m_tied1, svuint32_t,
		z0 = svlsl_u32_m (p0, z0, z1),
		z0 = svlsl_m (p0, z0, z1))

/*
** lsl_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_m_tied2, svuint32_t,
		z0 = svlsl_u32_m (p0, z1, z0),
		z0 = svlsl_m (p0, z1, z0))

/*
** lsl_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_m_untied, svuint32_t,
		z0 = svlsl_u32_m (p0, z1, z2),
		z0 = svlsl_m (p0, z1, z2))

/*
** lsl_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_m (p0, z0, x0),
		 z0 = svlsl_m (p0, z0, x0))

/*
** lsl_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_m (p0, z1, x0),
		 z0 = svlsl_m (p0, z1, x0))

/*
** lsl_1_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_m_tied1, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z0, 1),
		z0 = svlsl_m (p0, z0, 1))

/*
** lsl_1_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_m_untied, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z1, 1),
		z0 = svlsl_m (p0, z1, 1))

/*
** lsl_31_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_m_tied1, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z0, 31),
		z0 = svlsl_m (p0, z0, 31))

/*
** lsl_31_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_m_untied, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z1, 31),
		z0 = svlsl_m (p0, z1, 31))

/*
** lsl_32_u32_m_tied1:
**	mov	(z[0-9]+\.s), #32
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_m_tied1, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z0, 32),
		z0 = svlsl_m (p0, z0, 32))

/*
** lsl_32_u32_m_untied:
**	mov	(z[0-9]+\.s), #32
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_m_untied, svuint32_t,
		z0 = svlsl_n_u32_m (p0, z1, 32),
		z0 = svlsl_m (p0, z1, 32))

/*
** lsl_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_z_tied1, svuint32_t,
		z0 = svlsl_u32_z (p0, z0, z1),
		z0 = svlsl_z (p0, z0, z1))

/*
** lsl_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_z_tied2, svuint32_t,
		z0 = svlsl_u32_z (p0, z1, z0),
		z0 = svlsl_z (p0, z1, z0))

/*
** lsl_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_z_untied, svuint32_t,
		z0 = svlsl_u32_z (p0, z1, z2),
		z0 = svlsl_z (p0, z1, z2))

/*
** lsl_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_z (p0, z0, x0),
		 z0 = svlsl_z (p0, z0, x0))

/*
** lsl_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_z (p0, z1, x0),
		 z0 = svlsl_z (p0, z1, x0))

/*
** lsl_1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_z_tied1, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z0, 1),
		z0 = svlsl_z (p0, z0, 1))

/*
** lsl_1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_z_untied, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z1, 1),
		z0 = svlsl_z (p0, z1, 1))

/*
** lsl_31_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_z_tied1, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z0, 31),
		z0 = svlsl_z (p0, z0, 31))

/*
** lsl_31_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_z_untied, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z1, 31),
		z0 = svlsl_z (p0, z1, 31))

/*
** lsl_32_u32_z_tied1:
**	mov	(z[0-9]+\.s), #32
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_z_tied1, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z0, 32),
		z0 = svlsl_z (p0, z0, 32))

/*
** lsl_32_u32_z_untied:
**	mov	(z[0-9]+\.s), #32
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_z_untied, svuint32_t,
		z0 = svlsl_n_u32_z (p0, z1, 32),
		z0 = svlsl_z (p0, z1, 32))

/*
** lsl_u32_x_tied1:
**	lsl	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_x_tied1, svuint32_t,
		z0 = svlsl_u32_x (p0, z0, z1),
		z0 = svlsl_x (p0, z0, z1))

/*
** lsl_u32_x_tied2:
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_x_tied2, svuint32_t,
		z0 = svlsl_u32_x (p0, z1, z0),
		z0 = svlsl_x (p0, z1, z0))

/*
** lsl_u32_x_untied:
** (
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u32_x_untied, svuint32_t,
		z0 = svlsl_u32_x (p0, z1, z2),
		z0 = svlsl_x (p0, z1, z2))

/*
** lsl_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_x (p0, z0, x0),
		 z0 = svlsl_x (p0, z0, x0))

/*
** lsl_w0_u32_x_untied:
**	mov	z0\.s, w0
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svlsl_n_u32_x (p0, z1, x0),
		 z0 = svlsl_x (p0, z1, x0))

/*
** lsl_1_u32_x_tied1:
**	lsl	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_x_tied1, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z0, 1),
		z0 = svlsl_x (p0, z0, 1))

/*
** lsl_1_u32_x_untied:
**	lsl	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u32_x_untied, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z1, 1),
		z0 = svlsl_x (p0, z1, 1))

/*
** lsl_31_u32_x_tied1:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_x_tied1, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z0, 31),
		z0 = svlsl_x (p0, z0, 31))

/*
** lsl_31_u32_x_untied:
**	lsl	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_31_u32_x_untied, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z1, 31),
		z0 = svlsl_x (p0, z1, 31))

/*
** lsl_32_u32_x_tied1:
**	mov	(z[0-9]+\.s), #32
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_x_tied1, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z0, 32),
		z0 = svlsl_x (p0, z0, 32))

/*
** lsl_32_u32_x_untied:
**	mov	z0\.s, #32
**	lslr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (lsl_32_u32_x_untied, svuint32_t,
		z0 = svlsl_n_u32_x (p0, z1, 32),
		z0 = svlsl_x (p0, z1, 32))
