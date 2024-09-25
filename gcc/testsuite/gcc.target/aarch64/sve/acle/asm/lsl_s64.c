/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_s64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_s64_m_tied1, svint64_t, svuint64_t,
	     z0 = svlsl_s64_m (p0, z0, z4),
	     z0 = svlsl_m (p0, z0, z4))

/*
** lsl_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_s64_m_tied2, svint64_t, svuint64_t,
		 z0_res = svlsl_s64_m (p0, z4, z0),
		 z0_res = svlsl_m (p0, z4, z0))

/*
** lsl_s64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_s64_m_untied, svint64_t, svuint64_t,
	     z0 = svlsl_s64_m (p0, z1, z4),
	     z0 = svlsl_m (p0, z1, z4))

/*
** lsl_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_s64_m_tied1, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_m (p0, z0, x0),
		 z0 = svlsl_m (p0, z0, x0))

/*
** lsl_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_s64_m_untied, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_m (p0, z1, x0),
		 z0 = svlsl_m (p0, z1, x0))

/*
** lsl_1_s64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_m_tied1, svint64_t,
		z0 = svlsl_n_s64_m (p0, z0, 1),
		z0 = svlsl_m (p0, z0, 1))

/*
** lsl_1_s64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_m_untied, svint64_t,
		z0 = svlsl_n_s64_m (p0, z1, 1),
		z0 = svlsl_m (p0, z1, 1))

/*
** lsl_63_s64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_m_tied1, svint64_t,
		z0 = svlsl_n_s64_m (p0, z0, 63),
		z0 = svlsl_m (p0, z0, 63))

/*
** lsl_63_s64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_m_untied, svint64_t,
		z0 = svlsl_n_s64_m (p0, z1, 63),
		z0 = svlsl_m (p0, z1, 63))

/*
** lsl_64_s64_m_tied1:
**	mov	(z[0-9]+\.d), #64
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_s64_m_tied1, svint64_t,
		z0 = svlsl_n_s64_m (p0, z0, 64),
		z0 = svlsl_m (p0, z0, 64))

/*
** lsl_64_s64_m_untied:
**	mov	(z[0-9]+\.d), #64
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_s64_m_untied, svint64_t,
		z0 = svlsl_n_s64_m (p0, z1, 64),
		z0 = svlsl_m (p0, z1, 64))

/*
** lsl_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_s64_z_tied1, svint64_t, svuint64_t,
	     z0 = svlsl_s64_z (p0, z0, z4),
	     z0 = svlsl_z (p0, z0, z4))

/*
** lsl_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	lslr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (lsl_s64_z_tied2, svint64_t, svuint64_t,
		 z0_res = svlsl_s64_z (p0, z4, z0),
		 z0_res = svlsl_z (p0, z4, z0))

/*
** lsl_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (lsl_s64_z_untied, svint64_t, svuint64_t,
	     z0 = svlsl_s64_z (p0, z1, z4),
	     z0 = svlsl_z (p0, z1, z4))

/*
** lsl_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_s64_z_tied1, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_z (p0, z0, x0),
		 z0 = svlsl_z (p0, z0, x0))

/*
** lsl_x0_s64_z_untied:
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
TEST_UNIFORM_ZX (lsl_x0_s64_z_untied, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_z (p0, z1, x0),
		 z0 = svlsl_z (p0, z1, x0))

/*
** lsl_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_z_tied1, svint64_t,
		z0 = svlsl_n_s64_z (p0, z0, 1),
		z0 = svlsl_z (p0, z0, 1))

/*
** lsl_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_z_untied, svint64_t,
		z0 = svlsl_n_s64_z (p0, z1, 1),
		z0 = svlsl_z (p0, z1, 1))

/*
** lsl_63_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_z_tied1, svint64_t,
		z0 = svlsl_n_s64_z (p0, z0, 63),
		z0 = svlsl_z (p0, z0, 63))

/*
** lsl_63_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_z_untied, svint64_t,
		z0 = svlsl_n_s64_z (p0, z1, 63),
		z0 = svlsl_z (p0, z1, 63))

/*
** lsl_64_s64_z_tied1:
**	mov	(z[0-9]+\.d), #64
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_s64_z_tied1, svint64_t,
		z0 = svlsl_n_s64_z (p0, z0, 64),
		z0 = svlsl_z (p0, z0, 64))

/*
** lsl_64_s64_z_untied:
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
TEST_UNIFORM_Z (lsl_64_s64_z_untied, svint64_t,
		z0 = svlsl_n_s64_z (p0, z1, 64),
		z0 = svlsl_z (p0, z1, 64))

/*
** lsl_s64_x_tied1:
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_s64_x_tied1, svint64_t, svuint64_t,
	     z0 = svlsl_s64_x (p0, z0, z4),
	     z0 = svlsl_x (p0, z0, z4))

/*
** lsl_s64_x_tied2:
**	lslr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (lsl_s64_x_tied2, svint64_t, svuint64_t,
		 z0_res = svlsl_s64_x (p0, z4, z0),
		 z0_res = svlsl_x (p0, z4, z0))

/*
** lsl_s64_x_untied:
** (
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (lsl_s64_x_untied, svint64_t, svuint64_t,
	     z0 = svlsl_s64_x (p0, z1, z4),
	     z0 = svlsl_x (p0, z1, z4))

/*
** lsl_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_s64_x_tied1, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_x (p0, z0, x0),
		 z0 = svlsl_x (p0, z0, x0))

/*
** lsl_x0_s64_x_untied:
**	mov	z0\.d, x0
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (lsl_x0_s64_x_untied, svint64_t, uint64_t,
		 z0 = svlsl_n_s64_x (p0, z1, x0),
		 z0 = svlsl_x (p0, z1, x0))

/*
** lsl_1_s64_x_tied1:
**	add	z0\.d, z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_x_tied1, svint64_t,
		z0 = svlsl_n_s64_x (p0, z0, 1),
		z0 = svlsl_x (p0, z0, 1))

/*
** lsl_1_s64_x_untied:
**	add	z0\.d, z1\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_1_s64_x_untied, svint64_t,
		z0 = svlsl_n_s64_x (p0, z1, 1),
		z0 = svlsl_x (p0, z1, 1))

/*
** lsl_63_s64_x_tied1:
**	lsl	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_x_tied1, svint64_t,
		z0 = svlsl_n_s64_x (p0, z0, 63),
		z0 = svlsl_x (p0, z0, 63))

/*
** lsl_63_s64_x_untied:
**	lsl	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (lsl_63_s64_x_untied, svint64_t,
		z0 = svlsl_n_s64_x (p0, z1, 63),
		z0 = svlsl_x (p0, z1, 63))

/*
** lsl_64_s64_x_tied1:
**	mov	(z[0-9]+\.d), #64
**	lsl	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_64_s64_x_tied1, svint64_t,
		z0 = svlsl_n_s64_x (p0, z0, 64),
		z0 = svlsl_x (p0, z0, 64))

/*
** lsl_64_s64_x_untied:
**	mov	z0\.d, #64
**	lslr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (lsl_64_s64_x_untied, svint64_t,
		z0 = svlsl_n_s64_x (p0, z1, 64),
		z0 = svlsl_x (p0, z1, 64))
