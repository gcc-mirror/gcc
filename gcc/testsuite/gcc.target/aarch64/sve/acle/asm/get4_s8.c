/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get4_s8_z0_0:
**	mov	z0\.d, z4\.d
**	ret
*/
TEST_GET (get4_s8_z0_0, svint8x4_t, svint8_t,
	  z0 = svget4_s8 (z4, 0),
	  z0 = svget4 (z4, 0))

/*
** get4_s8_z0_1:
**	mov	z0\.d, z5\.d
**	ret
*/
TEST_GET (get4_s8_z0_1, svint8x4_t, svint8_t,
	  z0 = svget4_s8 (z4, 1),
	  z0 = svget4 (z4, 1))

/*
** get4_s8_z0_2:
**	mov	z0\.d, z6\.d
**	ret
*/
TEST_GET (get4_s8_z0_2, svint8x4_t, svint8_t,
	  z0 = svget4_s8 (z4, 2),
	  z0 = svget4 (z4, 2))

/*
** get4_s8_z0_3:
**	mov	z0\.d, z7\.d
**	ret
*/
TEST_GET (get4_s8_z0_3, svint8x4_t, svint8_t,
	  z0 = svget4_s8 (z4, 3),
	  z0 = svget4 (z4, 3))

/*
** get4_s8_z4_0:
**	ret
*/
TEST_GET (get4_s8_z4_0, svint8x4_t, svint8_t,
	  z4_res = svget4_s8 (z4, 0),
	  z4_res = svget4 (z4, 0))

/*
** get4_s8_z4_1:
**	mov	z4\.d, z5\.d
**	ret
*/
TEST_GET (get4_s8_z4_1, svint8x4_t, svint8_t,
	  z4_res = svget4_s8 (z4, 1),
	  z4_res = svget4 (z4, 1))

/*
** get4_s8_z4_2:
**	mov	z4\.d, z6\.d
**	ret
*/
TEST_GET (get4_s8_z4_2, svint8x4_t, svint8_t,
	  z4_res = svget4_s8 (z4, 2),
	  z4_res = svget4 (z4, 2))

/*
** get4_s8_z4_3:
**	mov	z4\.d, z7\.d
**	ret
*/
TEST_GET (get4_s8_z4_3, svint8x4_t, svint8_t,
	  z4_res = svget4_s8 (z4, 3),
	  z4_res = svget4 (z4, 3))

/*
** get4_s8_z5_0:
**	mov	z5\.d, z4\.d
**	ret
*/
TEST_GET (get4_s8_z5_0, svint8x4_t, svint8_t,
	  z5_res = svget4_s8 (z4, 0),
	  z5_res = svget4 (z4, 0))

/*
** get4_s8_z5_1:
**	ret
*/
TEST_GET (get4_s8_z5_1, svint8x4_t, svint8_t,
	  z5_res = svget4_s8 (z4, 1),
	  z5_res = svget4 (z4, 1))

/*
** get4_s8_z5_2:
**	mov	z5\.d, z6\.d
**	ret
*/
TEST_GET (get4_s8_z5_2, svint8x4_t, svint8_t,
	  z5_res = svget4_s8 (z4, 2),
	  z5_res = svget4 (z4, 2))

/*
** get4_s8_z5_3:
**	mov	z5\.d, z7\.d
**	ret
*/
TEST_GET (get4_s8_z5_3, svint8x4_t, svint8_t,
	  z5_res = svget4_s8 (z4, 3),
	  z5_res = svget4 (z4, 3))

/*
** get4_s8_z6_0:
**	mov	z6\.d, z4\.d
**	ret
*/
TEST_GET (get4_s8_z6_0, svint8x4_t, svint8_t,
	  z6_res = svget4_s8 (z4, 0),
	  z6_res = svget4 (z4, 0))

/*
** get4_s8_z6_1:
**	mov	z6\.d, z5\.d
**	ret
*/
TEST_GET (get4_s8_z6_1, svint8x4_t, svint8_t,
	  z6_res = svget4_s8 (z4, 1),
	  z6_res = svget4 (z4, 1))

/*
** get4_s8_z6_2:
**	ret
*/
TEST_GET (get4_s8_z6_2, svint8x4_t, svint8_t,
	  z6_res = svget4_s8 (z4, 2),
	  z6_res = svget4 (z4, 2))

/*
** get4_s8_z6_3:
**	mov	z6\.d, z7\.d
**	ret
*/
TEST_GET (get4_s8_z6_3, svint8x4_t, svint8_t,
	  z6_res = svget4_s8 (z4, 3),
	  z6_res = svget4 (z4, 3))

/*
** get4_s8_z7_0:
**	mov	z7\.d, z4\.d
**	ret
*/
TEST_GET (get4_s8_z7_0, svint8x4_t, svint8_t,
	  z7_res = svget4_s8 (z4, 0),
	  z7_res = svget4 (z4, 0))

/*
** get4_s8_z7_1:
**	mov	z7\.d, z5\.d
**	ret
*/
TEST_GET (get4_s8_z7_1, svint8x4_t, svint8_t,
	  z7_res = svget4_s8 (z4, 1),
	  z7_res = svget4 (z4, 1))

/*
** get4_s8_z7_2:
**	mov	z7\.d, z6\.d
**	ret
*/
TEST_GET (get4_s8_z7_2, svint8x4_t, svint8_t,
	  z7_res = svget4_s8 (z4, 2),
	  z7_res = svget4 (z4, 2))

/*
** get4_s8_z7_3:
**	ret
*/
TEST_GET (get4_s8_z7_3, svint8x4_t, svint8_t,
	  z7_res = svget4_s8 (z4, 3),
	  z7_res = svget4 (z4, 3))
