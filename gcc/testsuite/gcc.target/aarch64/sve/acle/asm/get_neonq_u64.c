/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** get_neonq_u64_z0:
**	mov	v0.16b, v4.16b
**	ret
*/
TEST_GET (get_neonq_u64_z0, svuint64_t, uint64x2_t,
	  z0 = svget_neonq_u64 (z4),
	  z0 = svget_neonq (z4))

/*
** get_neonq_u64_z4:
**	ret
*/
TEST_GET (get_neonq_u64_z4, svuint64_t, uint64x2_t,
	  z4_res = svget_neonq_u64 (z4),
	  z4_res = svget_neonq (z4))

/*
** get_neonq_u64_z5:
**	(
**	mov	z5.d, z4.d
**	|
**	mov	v5.16b, v4.16b
**	)
**	ret
*/
TEST_GET (get_neonq_u64_z5, svuint64_t, uint64x2_t,
	  z5_res = svget_neonq_u64 (z4),
	  z5_res = svget_neonq (z4))