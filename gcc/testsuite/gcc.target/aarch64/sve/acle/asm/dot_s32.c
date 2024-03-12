/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dot_s32_tied1:
**	sdot	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_s32_tied1, svint32_t, svint8_t,
	     z0 = svdot_s32 (z0, z4, z5),
	     z0 = svdot (z0, z4, z5))

/*
** dot_s32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.s, \1\.b, z1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_s32_tied2, svint32_t, svint8_t,
		 z0_res = svdot_s32 (z4, z0, z1),
		 z0_res = svdot (z4, z0, z1))

/*
** dot_s32_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.s, z1\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (dot_s32_tied3, svint32_t, svint8_t,
		 z0_res = svdot_s32 (z4, z1, z0),
		 z0_res = svdot (z4, z1, z0))

/*
** dot_s32_untied:
**	movprfx	z0, z1
**	sdot	z0\.s, z4\.b, z5\.b
**	ret
*/
TEST_DUAL_Z (dot_s32_untied, svint32_t, svint8_t,
	     z0 = svdot_s32 (z1, z4, z5),
	     z0 = svdot (z1, z4, z5))

/*
** dot_w0_s32_tied1:
**	mov	(z[0-9]+\.b), w0
**	sdot	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_s32_tied1, svint32_t, svint8_t, int8_t,
	      z0 = svdot_n_s32 (z0, z4, x0),
	      z0 = svdot (z0, z4, x0))

/*
** dot_w0_s32_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	sdot	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_s32_untied, svint32_t, svint8_t, int8_t,
	      z0 = svdot_n_s32 (z1, z4, x0),
	      z0 = svdot (z1, z4, x0))

/*
** dot_9_s32_tied1:
**	mov	(z[0-9]+\.b), #9
**	sdot	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (dot_9_s32_tied1, svint32_t, svint8_t,
	     z0 = svdot_n_s32 (z0, z4, 9),
	     z0 = svdot (z0, z4, 9))

/*
** dot_9_s32_untied:
**	mov	(z[0-9]+\.b), #9
**	movprfx	z0, z1
**	sdot	z0\.s, z4\.b, \1
**	ret
*/
TEST_DUAL_Z (dot_9_s32_untied, svint32_t, svint8_t,
	     z0 = svdot_n_s32 (z1, z4, 9),
	     z0 = svdot (z1, z4, 9))
