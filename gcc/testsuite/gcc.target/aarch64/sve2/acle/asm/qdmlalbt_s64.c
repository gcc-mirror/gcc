/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmlalbt_s64_tied1:
**	sqdmlalbt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (qdmlalbt_s64_tied1, svint64_t, svint32_t,
	     z0 = svqdmlalbt_s64 (z0, z4, z5),
	     z0 = svqdmlalbt (z0, z4, z5))

/*
** qdmlalbt_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalbt	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (qdmlalbt_s64_tied2, svint64_t, svint32_t,
		 z0_res = svqdmlalbt_s64 (z4, z0, z1),
		 z0_res = svqdmlalbt (z4, z0, z1))

/*
** qdmlalbt_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlalbt	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (qdmlalbt_s64_tied3, svint64_t, svint32_t,
		 z0_res = svqdmlalbt_s64 (z4, z1, z0),
		 z0_res = svqdmlalbt (z4, z1, z0))

/*
** qdmlalbt_s64_untied:
**	movprfx	z0, z1
**	sqdmlalbt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (qdmlalbt_s64_untied, svint64_t, svint32_t,
	     z0 = svqdmlalbt_s64 (z1, z4, z5),
	     z0 = svqdmlalbt (z1, z4, z5))

/*
** qdmlalbt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqdmlalbt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalbt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
	      z0 = svqdmlalbt_n_s64 (z0, z4, x0),
	      z0 = svqdmlalbt (z0, z4, x0))

/*
** qdmlalbt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sqdmlalbt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (qdmlalbt_w0_s64_untied, svint64_t, svint32_t, int32_t,
	      z0 = svqdmlalbt_n_s64 (z1, z4, x0),
	      z0 = svqdmlalbt (z1, z4, x0))

/*
** qdmlalbt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sqdmlalbt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (qdmlalbt_11_s64_tied1, svint64_t, svint32_t,
	     z0 = svqdmlalbt_n_s64 (z0, z4, 11),
	     z0 = svqdmlalbt (z0, z4, 11))

/*
** qdmlalbt_11_s64_untied:
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	sqdmlalbt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (qdmlalbt_11_s64_untied, svint64_t, svint32_t,
	     z0 = svqdmlalbt_n_s64 (z1, z4, 11),
	     z0 = svqdmlalbt (z1, z4, 11))
