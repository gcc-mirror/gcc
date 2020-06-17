/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sra_1_s16_tied1:
**	ssra	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_s16_tied1, svint16_t,
		z0 = svsra_n_s16 (z0, z1, 1),
		z0 = svsra (z0, z1, 1))

/*
** sra_1_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ssra	z0\.h, \1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_s16_tied2, svint16_t,
		z0 = svsra_n_s16 (z1, z0, 1),
		z0 = svsra (z1, z0, 1))

/*
** sra_1_s16_untied:
**	movprfx	z0, z1
**	ssra	z0\.h, z2\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_s16_untied, svint16_t,
		z0 = svsra_n_s16 (z1, z2, 1),
		z0 = svsra (z1, z2, 1))

/*
** sra_2_s16_tied1:
**	ssra	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_s16_tied1, svint16_t,
		z0 = svsra_n_s16 (z0, z1, 2),
		z0 = svsra (z0, z1, 2))

/*
** sra_2_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ssra	z0\.h, \1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_s16_tied2, svint16_t,
		z0 = svsra_n_s16 (z1, z0, 2),
		z0 = svsra (z1, z0, 2))

/*
** sra_2_s16_untied:
**	movprfx	z0, z1
**	ssra	z0\.h, z2\.h, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_s16_untied, svint16_t,
		z0 = svsra_n_s16 (z1, z2, 2),
		z0 = svsra (z1, z2, 2))

/*
** sra_16_s16_tied1:
**	ssra	z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (sra_16_s16_tied1, svint16_t,
		z0 = svsra_n_s16 (z0, z1, 16),
		z0 = svsra (z0, z1, 16))

/*
** sra_16_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ssra	z0\.h, \1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (sra_16_s16_tied2, svint16_t,
		z0 = svsra_n_s16 (z1, z0, 16),
		z0 = svsra (z1, z0, 16))

/*
** sra_16_s16_untied:
**	movprfx	z0, z1
**	ssra	z0\.h, z2\.h, #16
**	ret
*/
TEST_UNIFORM_Z (sra_16_s16_untied, svint16_t,
		z0 = svsra_n_s16 (z1, z2, 16),
		z0 = svsra (z1, z2, 16))
