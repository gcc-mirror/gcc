/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_m_tied1, svuint64_t,
		z0 = svlsl_u64_m (p0, z0, z1),
		z0 = svlsl_m (p0, z0, z1))

/*
** lsl_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_m_tied2, svuint64_t,
		z0 = svlsl_u64_m (p0, z1, z0),
		z0 = svlsl_m (p0, z1, z0))

/*
** lsl_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_m_untied, svuint64_t,
		z0 = svlsl_u64_m (p0, z1, z2),
		z0 = svlsl_m (p0, z1, z2))

/*
** lsl_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_m (p0, z0, x0),
		 z0 = svlsl_m (p0, z0, x0))

/*
** lsl_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_m (p0, z1, x0),
		 z0 = svlsl_m (p0, z1, x0))

/*
** lsl_1_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_m_tied1, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z0, 1),
		z0 = svlsl_m (p0, z0, 1))

/*
** lsl_1_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_m_untied, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z1, 1),
		z0 = svlsl_m (p0, z1, 1))

/*
** lsl_63_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_m_tied1, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z0, 63),
		z0 = svlsl_m (p0, z0, 63))

/*
** lsl_63_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_m_untied, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z1, 63),
		z0 = svlsl_m (p0, z1, 63))

/*
** lsl_64_u64_m_tied1:
**	mov	(z[0-9]+\.d), #64
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_m_tied1, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z0, 64),
		z0 = svlsl_m (p0, z0, 64))

/*
** lsl_64_u64_m_untied:
**	mov	(z[0-9]+\.d), #64
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_m_untied, svuint64_t,
		z0 = svlsl_n_u64_m (p0, z1, 64),
		z0 = svlsl_m (p0, z1, 64))

/*
** lsl_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_z_tied1, svuint64_t,
		z0 = svlsl_u64_z (p0, z0, z1),
		z0 = svlsl_z (p0, z0, z1))

/*
** lsl_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_z_tied2, svuint64_t,
		z0 = svlsl_u64_z (p0, z1, z0),
		z0 = svlsl_z (p0, z1, z0))

/*
** lsl_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_z_untied, svuint64_t,
		z0 = svlsl_u64_z (p0, z1, z2),
		z0 = svlsl_z (p0, z1, z2))

/*
** lsl_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_z (p0, z0, x0),
		 z0 = svlsl_z (p0, z0, x0))

/*
** lsl_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_z (p0, z1, x0),
		 z0 = svlsl_z (p0, z1, x0))

/*
** lsl_1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_z_tied1, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z0, 1),
		z0 = svlsl_z (p0, z0, 1))

/*
** lsl_1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_z_untied, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z1, 1),
		z0 = svlsl_z (p0, z1, 1))

/*
** lsl_63_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_z_tied1, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z0, 63),
		z0 = svlsl_z (p0, z0, 63))

/*
** lsl_63_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_z_untied, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z1, 63),
		z0 = svlsl_z (p0, z1, 63))

/*
** lsl_64_u64_z_tied1:
**	mov	(z[0-9]+\.d), #64
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_z_tied1, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z0, 64),
		z0 = svlsl_z (p0, z0, 64))

/*
** lsl_64_u64_z_untied:
**	mov	(z[0-9]+\.d), #64
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_z_untied, svuint64_t,
		z0 = svlsl_n_u64_z (p0, z1, 64),
		z0 = svlsl_z (p0, z1, 64))

/*
** lsl_u64_x_tied1:
**	lsl	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_x_tied1, svuint64_t,
		z0 = svlsl_u64_x (p0, z0, z1),
		z0 = svlsl_x (p0, z0, z1))

/*
** lsl_u64_x_tied2:
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_x_tied2, svuint64_t,
		z0 = svlsl_u64_x (p0, z1, z0),
		z0 = svlsl_x (p0, z1, z0))

/*
** lsl_u64_x_untied:
** (
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u64_x_untied, svuint64_t,
		z0 = svlsl_u64_x (p0, z1, z2),
		z0 = svlsl_x (p0, z1, z2))

/*
** lsl_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_x (p0, z0, x0),
		 z0 = svlsl_x (p0, z0, x0))

/*
** lsl_x0_u64_x_untied:
**	mov	z0\.d, x0
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svlsl_n_u64_x (p0, z1, x0),
		 z0 = svlsl_x (p0, z1, x0))

/*
** lsl_1_u64_x_tied1:
**	lsl	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_x_tied1, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z0, 1),
		z0 = svlsl_x (p0, z0, 1))

/*
** lsl_1_u64_x_untied:
**	lsl	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u64_x_untied, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z1, 1),
		z0 = svlsl_x (p0, z1, 1))

/*
** lsl_63_u64_x_tied1:
**	lsl	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_x_tied1, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z0, 63),
		z0 = svlsl_x (p0, z0, 63))

/*
** lsl_63_u64_x_untied:
**	lsl	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_u64_x_untied, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z1, 63),
		z0 = svlsl_x (p0, z1, 63))

/*
** lsl_64_u64_x_tied1:
**	mov	(z[0-9]+\.d), #64
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_x_tied1, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z0, 64),
		z0 = svlsl_x (p0, z0, 64))

/*
** lsl_64_u64_x_untied:
**	mov	z0\.d, #64
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_64_u64_x_untied, svuint64_t,
		z0 = svlsl_n_u64_x (p0, z1, 64),
		z0 = svlsl_x (p0, z1, 64))
