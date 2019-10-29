/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get2_u8_z0_0:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_GET (get2_u8_z0_0, svuint8x2_t, svuint8_t,
	  z0 = svget2_u8 (z4, 0),
	  z0 = svget2 (z4, 0))

/*
** get2_u8_z0_1:
**	mov	z0\.d, z5\.d
**	ret
*/
TEST_GET (get2_u8_z0_1, svuint8x2_t, svuint8_t,
	  z0 = svget2_u8 (z4, 1),
	  z0 = svget2 (z4, 1))

/*
** get2_u8_z4_0:
**	ret
*/
TEST_GET (get2_u8_z4_0, svuint8x2_t, svuint8_t,
	  z4_res = svget2_u8 (z4, 0),
	  z4_res = svget2 (z4, 0))

/*
** get2_u8_z4_1:
**	mov	z4\.d, z5\.d
**	ret
*/
TEST_GET (get2_u8_z4_1, svuint8x2_t, svuint8_t,
	  z4_res = svget2_u8 (z4, 1),
	  z4_res = svget2 (z4, 1))

/*
** get2_u8_z5_0:
**	mov	z5\.d, z4\.d
**	ret
*/
TEST_GET (get2_u8_z5_0, svuint8x2_t, svuint8_t,
	  z5_res = svget2_u8 (z4, 0),
	  z5_res = svget2 (z4, 0))

/*
** get2_u8_z5_1:
**	ret
*/
TEST_GET (get2_u8_z5_1, svuint8x2_t, svuint8_t,
	  z5_res = svget2_u8 (z4, 1),
	  z5_res = svget2 (z4, 1))
