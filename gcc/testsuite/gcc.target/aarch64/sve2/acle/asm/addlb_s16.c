/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addlb_s16_tied1:
**	saddlb	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlb_s16 (z0, z1),
		    z0_res = svaddlb (z0, z1))

/*
** addlb_s16_tied2:
**	saddlb	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_s16_tied2, svint16_t, svint8_t,
		    z0_res = svaddlb_s16 (z1, z0),
		    z0_res = svaddlb (z1, z0))

/*
** addlb_s16_untied:
**	saddlb	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlb_s16 (z1, z2),
		    z0_res = svaddlb (z1, z2))

/*
** addlb_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	saddlb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlb_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlb_n_s16 (z0, x0),
		     z0_res = svaddlb (z0, x0))

/*
** addlb_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	saddlb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (addlb_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svaddlb_n_s16 (z1, x0),
		     z0_res = svaddlb (z1, x0))

/*
** addlb_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	saddlb	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svaddlb_n_s16 (z0, 11),
		    z0_res = svaddlb (z0, 11))

/*
** addlb_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	saddlb	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (addlb_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svaddlb_n_s16 (z1, 11),
		    z0_res = svaddlb (z1, 11))
