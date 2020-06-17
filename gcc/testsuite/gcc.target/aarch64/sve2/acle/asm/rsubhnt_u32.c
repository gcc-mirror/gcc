/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnt_u32_tied1:
**	rsubhnt	z0\.h, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (rsubhnt_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svrsubhnt_u32 (z0, z4, z5),
	     z0 = svrsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_u32_tied2, svuint16_t, svuint32_t,
		 z0_res = svrsubhnt_u32 (z4, z0, z1),
		 z0_res = svrsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_u32_tied3, svuint16_t, svuint32_t,
		 z0_res = svrsubhnt_u32 (z4, z1, z0),
		 z0_res = svrsubhnt (z4, z1, z0))

/*
** rsubhnt_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.h, z4\.s, z5\.s
** |
**	rsubhnt	z1\.h, z4\.s, z5\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_u32_untied, svuint16_t, svuint32_t,
	     z0 = svrsubhnt_u32 (z1, z4, z5),
	     z0 = svrsubhnt (z1, z4, z5))

/*
** rsubhnt_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	rsubhnt	z0\.h, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (rsubhnt_w0_u32_tied1, svuint16_t, svuint32_t, uint32_t,
	      z0 = svrsubhnt_n_u32 (z0, z4, x0),
	      z0 = svrsubhnt (z0, z4, x0))

/*
** rsubhnt_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.h, z4\.s, \1
** |
**	rsubhnt	z1\.h, z4\.s, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (rsubhnt_w0_u32_untied, svuint16_t, svuint32_t, uint32_t,
	      z0 = svrsubhnt_n_u32 (z1, z4, x0),
	      z0 = svrsubhnt (z1, z4, x0))

/*
** rsubhnt_11_u32_tied1:
**	mov	(z[0-9]+\.s), #11
**	rsubhnt	z0\.h, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svrsubhnt_n_u32 (z0, z4, 11),
	     z0 = svrsubhnt (z0, z4, 11))

/*
** rsubhnt_11_u32_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.h, z4\.s, \1
** |
**	rsubhnt	z1\.h, z4\.s, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_u32_untied, svuint16_t, svuint32_t,
	     z0 = svrsubhnt_n_u32 (z1, z4, 11),
	     z0 = svrsubhnt (z1, z4, 11))
