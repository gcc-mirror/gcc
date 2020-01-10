/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cdot_0_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b, #0
**	ret
*/
TEST_DUAL_Z (cdot_0_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z0, z4, z5, 0),
	     z0 = svcdot (z0, z4, z5, 0))

/*
** cdot_0_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_0_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z0, z1, 0),
		 z0_res = svcdot (z4, z0, z1, 0))

/*
** cdot_0_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (cdot_0_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z1, z0, 0),
		 z0_res = svcdot (z4, z1, z0, 0))

/*
** cdot_0_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b, #0
**	ret
*/
TEST_DUAL_Z (cdot_0_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z1, z4, z5, 0),
	     z0 = svcdot (z1, z4, z5, 0))

/*
** cdot_90_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b, #90
**	ret
*/
TEST_DUAL_Z (cdot_90_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z0, z4, z5, 90),
	     z0 = svcdot (z0, z4, z5, 90))

/*
** cdot_90_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b, #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_90_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z0, z1, 90),
		 z0_res = svcdot (z4, z0, z1, 90))

/*
** cdot_90_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b, #90
**	ret
*/
TEST_DUAL_Z_REV (cdot_90_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z1, z0, 90),
		 z0_res = svcdot (z4, z1, z0, 90))

/*
** cdot_90_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b, #90
**	ret
*/
TEST_DUAL_Z (cdot_90_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z1, z4, z5, 90),
	     z0 = svcdot (z1, z4, z5, 90))

/*
** cdot_180_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b, #180
**	ret
*/
TEST_DUAL_Z (cdot_180_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z0, z4, z5, 180),
	     z0 = svcdot (z0, z4, z5, 180))

/*
** cdot_180_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b, #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_180_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z0, z1, 180),
		 z0_res = svcdot (z4, z0, z1, 180))

/*
** cdot_180_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b, #180
**	ret
*/
TEST_DUAL_Z_REV (cdot_180_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z1, z0, 180),
		 z0_res = svcdot (z4, z1, z0, 180))

/*
** cdot_180_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b, #180
**	ret
*/
TEST_DUAL_Z (cdot_180_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z1, z4, z5, 180),
	     z0 = svcdot (z1, z4, z5, 180))

/*
** cdot_270_s32_tied1:
**	cdot	z0\.s, z4\.b, z5\.b, #270
**	ret
*/
TEST_DUAL_Z (cdot_270_s32_tied1, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z0, z4, z5, 270),
	     z0 = svcdot (z0, z4, z5, 270))

/*
** cdot_270_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, \1\.b, z1\.b, #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_270_s32_tied2, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z0, z1, 270),
		 z0_res = svcdot (z4, z0, z1, 270))

/*
** cdot_270_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	cdot	z0\.s, z1\.b, \1\.b, #270
**	ret
*/
TEST_DUAL_Z_REV (cdot_270_s32_tied3, svint32_t, svint8_t,
		 z0_res = svcdot_s32 (z4, z1, z0, 270),
		 z0_res = svcdot (z4, z1, z0, 270))

/*
** cdot_270_s32_untied:
**	movprfx	z0, z1
**	cdot	z0\.s, z4\.b, z5\.b, #270
**	ret
*/
TEST_DUAL_Z (cdot_270_s32_untied, svint32_t, svint8_t,
	     z0 = svcdot_s32 (z1, z4, z5, 270),
	     z0 = svcdot (z1, z4, z5, 270))
