/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbx_bf16_tied1:
**	tbx	z0\.h, z1\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (tbx_bf16_tied1, svbfloat16_t, svuint16_t,
	     z0 = svtbx_bf16 (z0, z1, z4),
	     z0 = svtbx (z0, z1, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z (tbx_bf16_tied2, svbfloat16_t, svuint16_t,
	     z0 = svtbx_bf16 (z1, z0, z4),
	     z0 = svtbx (z1, z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (tbx_bf16_tied3, svbfloat16_t, svuint16_t,
		 z0_res = svtbx_bf16 (z4, z5, z0),
		 z0_res = svtbx (z4, z5, z0))

/*
** tbx_bf16_untied:
** (
**	mov	z0\.d, z1\.d
**	tbx	z0\.h, z2\.h, z4\.h
** |
**	tbx	z1\.h, z2\.h, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (tbx_bf16_untied, svbfloat16_t, svuint16_t,
	     z0 = svtbx_bf16 (z1, z2, z4),
	     z0 = svtbx (z1, z2, z4))
