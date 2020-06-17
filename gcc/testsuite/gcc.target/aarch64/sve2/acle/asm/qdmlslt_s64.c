/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmlslt_s64_tied1:
**	sqdmlslt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (qdmlslt_s64_tied1, svint64_t, svint32_t,
	     z0 = svqdmlslt_s64 (z0, z4, z5),
	     z0 = svqdmlslt (z0, z4, z5))

/*
** qdmlslt_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlslt	z0\.d, \1\.s, z1\.s
**	ret
*/
TEST_DUAL_Z_REV (qdmlslt_s64_tied2, svint64_t, svint32_t,
		 z0_res = svqdmlslt_s64 (z4, z0, z1),
		 z0_res = svqdmlslt (z4, z0, z1))

/*
** qdmlslt_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sqdmlslt	z0\.d, z1\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (qdmlslt_s64_tied3, svint64_t, svint32_t,
		 z0_res = svqdmlslt_s64 (z4, z1, z0),
		 z0_res = svqdmlslt (z4, z1, z0))

/*
** qdmlslt_s64_untied:
**	movprfx	z0, z1
**	sqdmlslt	z0\.d, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (qdmlslt_s64_untied, svint64_t, svint32_t,
	     z0 = svqdmlslt_s64 (z1, z4, z5),
	     z0 = svqdmlslt (z1, z4, z5))

/*
** qdmlslt_w0_s64_tied1:
**	mov	(z[0-9]+\.s), w0
**	sqdmlslt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (qdmlslt_w0_s64_tied1, svint64_t, svint32_t, int32_t,
	      z0 = svqdmlslt_n_s64 (z0, z4, x0),
	      z0 = svqdmlslt (z0, z4, x0))

/*
** qdmlslt_w0_s64_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	sqdmlslt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (qdmlslt_w0_s64_untied, svint64_t, svint32_t, int32_t,
	      z0 = svqdmlslt_n_s64 (z1, z4, x0),
	      z0 = svqdmlslt (z1, z4, x0))

/*
** qdmlslt_11_s64_tied1:
**	mov	(z[0-9]+\.s), #11
**	sqdmlslt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (qdmlslt_11_s64_tied1, svint64_t, svint32_t,
	     z0 = svqdmlslt_n_s64 (z0, z4, 11),
	     z0 = svqdmlslt (z0, z4, 11))

/*
** qdmlslt_11_s64_untied:: { xfail *-*-*}
**	mov	(z[0-9]+\.s), #11
**	movprfx	z0, z1
**	sqdmlslt	z0\.d, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (qdmlslt_11_s64_untied, svint64_t, svint32_t,
	     z0 = svqdmlslt_n_s64 (z1, z4, 11),
	     z0 = svqdmlslt (z1, z4, 11))
