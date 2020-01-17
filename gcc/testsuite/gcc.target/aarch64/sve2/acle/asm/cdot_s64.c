/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cdot_0_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h, #0
**	ret
*/
TEST_DUAL_Z (cdot_0_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z0, z4, z5, 0),
	     z0 = svcdot (z0, z4, z5, 0))

/*
** cdot_0_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_0_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z0, z1, 0),
		 z0_res = svcdot (z4, z0, z1, 0))

/*
** cdot_0_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_0_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z1, z0, 0),
		 z0_res = svcdot (z4, z1, z0, 0))

/*
** cdot_0_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h, #0
**	ret
*/
TEST_DUAL_Z (cdot_0_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z1, z4, z5, 0),
	     z0 = svcdot (z1, z4, z5, 0))

/*
** cdot_90_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h, #90
**	ret
*/
TEST_DUAL_Z (cdot_90_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z0, z4, z5, 90),
	     z0 = svcdot (z0, z4, z5, 90))

/*
** cdot_90_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h, #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_90_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z0, z1, 90),
		 z0_res = svcdot (z4, z0, z1, 90))

/*
** cdot_90_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h, #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_90_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z1, z0, 90),
		 z0_res = svcdot (z4, z1, z0, 90))

/*
** cdot_90_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h, #90
**	ret
*/
TEST_DUAL_Z (cdot_90_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z1, z4, z5, 90),
	     z0 = svcdot (z1, z4, z5, 90))

/*
** cdot_180_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h, #180
**	ret
*/
TEST_DUAL_Z (cdot_180_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z0, z4, z5, 180),
	     z0 = svcdot (z0, z4, z5, 180))

/*
** cdot_180_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h, #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_180_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z0, z1, 180),
		 z0_res = svcdot (z4, z0, z1, 180))

/*
** cdot_180_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h, #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_180_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z1, z0, 180),
		 z0_res = svcdot (z4, z1, z0, 180))

/*
** cdot_180_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h, #180
**	ret
*/
TEST_DUAL_Z (cdot_180_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z1, z4, z5, 180),
	     z0 = svcdot (z1, z4, z5, 180))

/*
** cdot_270_s64_tied1:
**	cdot	z0\.d, z4\.h, z5\.h, #270
**	ret
*/
TEST_DUAL_Z (cdot_270_s64_tied1, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z0, z4, z5, 270),
	     z0 = svcdot (z0, z4, z5, 270))

/*
** cdot_270_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, \1\.h, z1\.h, #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_270_s64_tied2, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z0, z1, 270),
		 z0_res = svcdot (z4, z0, z1, 270))

/*
** cdot_270_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.d, z1\.h, \1\.h, #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_270_s64_tied3, svint64_t, svint16_t,
		 z0_res = svcdot_s64 (z4, z1, z0, 270),
		 z0_res = svcdot (z4, z1, z0, 270))

/*
** cdot_270_s64_untied:
**	movprfx	z0, z1
**	cdot	z0\.d, z4\.h, z5\.h, #270
**	ret
*/
TEST_DUAL_Z (cdot_270_s64_untied, svint64_t, svint16_t,
	     z0 = svcdot_s64 (z1, z4, z5, 270),
	     z0 = svcdot (z1, z4, z5, 270))
