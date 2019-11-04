/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get3_u16_z0_0:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_GET (get3_u16_z0_0, svuint16x3_t, svuint16_t,
	  z0 = svget3_u16 (z4, 0),
	  z0 = svget3 (z4, 0))

/*
** get3_u16_z0_1:
**	mov	z0\.d, z5\.d
**	ret
*/
TEST_GET (get3_u16_z0_1, svuint16x3_t, svuint16_t,
	  z0 = svget3_u16 (z4, 1),
	  z0 = svget3 (z4, 1))

/*
** get3_u16_z0_2:
**	mov	z0\.d, z6\.d
**	ret
*/
TEST_GET (get3_u16_z0_2, svuint16x3_t, svuint16_t,
	  z0 = svget3_u16 (z4, 2),
	  z0 = svget3 (z4, 2))

/*
** get3_u16_z4_0:
**	ret
*/
TEST_GET (get3_u16_z4_0, svuint16x3_t, svuint16_t,
	  z4_res = svget3_u16 (z4, 0),
	  z4_res = svget3 (z4, 0))

/*
** get3_u16_z4_1:
**	mov	z4\.d, z5\.d
**	ret
*/
TEST_GET (get3_u16_z4_1, svuint16x3_t, svuint16_t,
	  z4_res = svget3_u16 (z4, 1),
	  z4_res = svget3 (z4, 1))

/*
** get3_u16_z4_2:
**	mov	z4\.d, z6\.d
**	ret
*/
TEST_GET (get3_u16_z4_2, svuint16x3_t, svuint16_t,
	  z4_res = svget3_u16 (z4, 2),
	  z4_res = svget3 (z4, 2))

/*
** get3_u16_z5_0:
**	mov	z5\.d, z4\.d
**	ret
*/
TEST_GET (get3_u16_z5_0, svuint16x3_t, svuint16_t,
	  z5_res = svget3_u16 (z4, 0),
	  z5_res = svget3 (z4, 0))

/*
** get3_u16_z5_1:
**	ret
*/
TEST_GET (get3_u16_z5_1, svuint16x3_t, svuint16_t,
	  z5_res = svget3_u16 (z4, 1),
	  z5_res = svget3 (z4, 1))

/*
** get3_u16_z5_2:
**	mov	z5\.d, z6\.d
**	ret
*/
TEST_GET (get3_u16_z5_2, svuint16x3_t, svuint16_t,
	  z5_res = svget3_u16 (z4, 2),
	  z5_res = svget3 (z4, 2))

/*
** get3_u16_z6_0:
**	mov	z6\.d, z4\.d
**	ret
*/
TEST_GET (get3_u16_z6_0, svuint16x3_t, svuint16_t,
	  z6_res = svget3_u16 (z4, 0),
	  z6_res = svget3 (z4, 0))

/*
** get3_u16_z6_1:
**	mov	z6\.d, z5\.d
**	ret
*/
TEST_GET (get3_u16_z6_1, svuint16x3_t, svuint16_t,
	  z6_res = svget3_u16 (z4, 1),
	  z6_res = svget3 (z4, 1))

/*
** get3_u16_z6_2:
**	ret
*/
TEST_GET (get3_u16_z6_2, svuint16x3_t, svuint16_t,
	  z6_res = svget3_u16 (z4, 2),
	  z6_res = svget3 (z4, 2))
