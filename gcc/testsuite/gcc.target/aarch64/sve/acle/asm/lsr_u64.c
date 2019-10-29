/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsr_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_m_tied1, svuint64_t,
		z0 = svlsr_u64_m (p0, z0, z1),
		z0 = svlsr_m (p0, z0, z1))

/*
** lsr_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_m_tied2, svuint64_t,
		z0 = svlsr_u64_m (p0, z1, z0),
		z0 = svlsr_m (p0, z1, z0))

/*
** lsr_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_m_untied, svuint64_t,
		z0 = svlsr_u64_m (p0, z1, z2),
		z0 = svlsr_m (p0, z1, z2))

/*
** lsr_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_m (p0, z0, x0),
		 z0 = svlsr_m (p0, z0, x0))

/*
** lsr_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_m (p0, z1, x0),
		 z0 = svlsr_m (p0, z1, x0))

/*
** lsr_1_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_m_tied1, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z0, 1),
		z0 = svlsr_m (p0, z0, 1))

/*
** lsr_1_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_m_untied, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z1, 1),
		z0 = svlsr_m (p0, z1, 1))

/*
** lsr_63_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_m_tied1, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z0, 63),
		z0 = svlsr_m (p0, z0, 63))

/*
** lsr_63_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_m_untied, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z1, 63),
		z0 = svlsr_m (p0, z1, 63))

/*
** lsr_64_u64_m_tied1:
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_m_tied1, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z0, 64),
		z0 = svlsr_m (p0, z0, 64))

/*
** lsr_64_u64_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_m_untied, svuint64_t,
		z0 = svlsr_n_u64_m (p0, z1, 64),
		z0 = svlsr_m (p0, z1, 64))

/*
** lsr_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_z_tied1, svuint64_t,
		z0 = svlsr_u64_z (p0, z0, z1),
		z0 = svlsr_z (p0, z0, z1))

/*
** lsr_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_z_tied2, svuint64_t,
		z0 = svlsr_u64_z (p0, z1, z0),
		z0 = svlsr_z (p0, z1, z0))

/*
** lsr_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_z_untied, svuint64_t,
		z0 = svlsr_u64_z (p0, z1, z2),
		z0 = svlsr_z (p0, z1, z2))

/*
** lsr_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_z (p0, z0, x0),
		 z0 = svlsr_z (p0, z0, x0))

/*
** lsr_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_z (p0, z1, x0),
		 z0 = svlsr_z (p0, z1, x0))

/*
** lsr_1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_z_tied1, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z0, 1),
		z0 = svlsr_z (p0, z0, 1))

/*
** lsr_1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_z_untied, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z1, 1),
		z0 = svlsr_z (p0, z1, 1))

/*
** lsr_63_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_z_tied1, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z0, 63),
		z0 = svlsr_z (p0, z0, 63))

/*
** lsr_63_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_z_untied, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z1, 63),
		z0 = svlsr_z (p0, z1, 63))

/*
** lsr_64_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_z_tied1, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z0, 64),
		z0 = svlsr_z (p0, z0, 64))

/*
** lsr_64_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_z_untied, svuint64_t,
		z0 = svlsr_n_u64_z (p0, z1, 64),
		z0 = svlsr_z (p0, z1, 64))

/*
** lsr_u64_x_tied1:
**	lsr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_x_tied1, svuint64_t,
		z0 = svlsr_u64_x (p0, z0, z1),
		z0 = svlsr_x (p0, z0, z1))

/*
** lsr_u64_x_tied2:
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_x_tied2, svuint64_t,
		z0 = svlsr_u64_x (p0, z1, z0),
		z0 = svlsr_x (p0, z1, z0))

/*
** lsr_u64_x_untied:
** (
**	movprfx	z0, z1
**	lsr	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u64_x_untied, svuint64_t,
		z0 = svlsr_u64_x (p0, z1, z2),
		z0 = svlsr_x (p0, z1, z2))

/*
** lsr_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_x (p0, z0, x0),
		 z0 = svlsr_x (p0, z0, x0))

/*
** lsr_x0_u64_x_untied:
**	mov	z0\.d, x0
**	lsrr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (lsr_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svlsr_n_u64_x (p0, z1, x0),
		 z0 = svlsr_x (p0, z1, x0))

/*
** lsr_1_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_x_tied1, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z0, 1),
		z0 = svlsr_x (p0, z0, 1))

/*
** lsr_1_u64_x_untied:
**	lsr	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u64_x_untied, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z1, 1),
		z0 = svlsr_x (p0, z1, 1))

/*
** lsr_63_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_x_tied1, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z0, 63),
		z0 = svlsr_x (p0, z0, 63))

/*
** lsr_63_u64_x_untied:
**	lsr	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsr_63_u64_x_untied, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z1, 63),
		z0 = svlsr_x (p0, z1, 63))

/*
** lsr_64_u64_x_tied1:
**	lsr	z0\.d, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_x_tied1, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z0, 64),
		z0 = svlsr_x (p0, z0, 64))

/*
** lsr_64_u64_x_untied:
**	lsr	z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (lsr_64_u64_x_untied, svuint64_t,
		z0 = svlsr_n_u64_x (p0, z1, 64),
		z0 = svlsr_x (p0, z1, 64))
