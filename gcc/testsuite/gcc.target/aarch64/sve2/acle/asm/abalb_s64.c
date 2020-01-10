/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abalb_s64_tied1:
**	sabalb	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abalb_s64_tied1, svint64_t, svint32_t,
	     z0 = svabalb_s64 (z0, z4, z5),
	     z0 = svabalb (z0, z4, z5))

/*
** abalb_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabalb	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (abalb_s64_tied2, svint64_t, svint32_t,
		 z0_res = svabalb_s64 (z4, z0, z1),
		 z0_res = svabalb (z4, z0, z1))

/*
** abalb_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sabalb	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (abalb_s64_tied3, svint64_t, svint32_t,
		 z0_res = svabalb_s64 (z4, z1, z0),
		 z0_res = svabalb (z4, z1, z0))

/*
** abalb_s64_untied:
**	movprfx	z0, z1
**	sabalb	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (abalb_s64_untied, svint64_t, svint32_t,
	     z0 = svabalb_s64 (z1, z4, z5),
	     z0 = svabalb (z1, z4, z5))

/*
** abalb_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sabalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abalb_w0_s64_tied1, svint64_t, svint32_t, int32_t,
	      z0 = svabalb_n_s64 (z0, z4, x0),
	      z0 = svabalb (z0, z4, x0))

/*
** abalb_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sabalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (abalb_w0_s64_untied, svint64_t, svint32_t, int32_t,
	      z0 = svabalb_n_s64 (z1, z4, x0),
	      z0 = svabalb (z1, z4, x0))

/*
** abalb_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sabalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abalb_11_s64_tied1, svint64_t, svint32_t,
	     z0 = svabalb_n_s64 (z0, z4, 11),
	     z0 = svabalb (z0, z4, 11))

/*
** abalb_11_s64_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	sabalb	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (abalb_11_s64_untied, svint64_t, svint32_t,
	     z0 = svabalb_n_s64 (z1, z4, 11),
	     z0 = svabalb (z1, z4, 11))
