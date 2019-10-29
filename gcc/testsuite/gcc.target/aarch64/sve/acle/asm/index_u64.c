/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** index_u64_x0_x1:
**	index	z0\.d, x0, x1
**	ret
*/
TEST_S (index_u64_x0_x1, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, x1))

/*
** index_u64_x0_2:
**	index	z0\.d, x0, #2
**	ret
*/
TEST_S (index_u64_x0_2, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, 2))

/*
** index_u64_50_2:
**	mov	(x[0-9]+), 50
**	index	z0\.d, \1, #2
**	ret
*/
TEST_S (index_u64_50_2, svuint64_t, uint64_t,
	z0 = svindex_u64 (50, 2))

/*
** index_u64_0_m17:
**	mov	(x[0-9]+), -17
**	index	z0\.d, #0, \1
**	ret
*/
TEST_S (index_u64_0_m17, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, -17))

/*
** index_u64_0_m16:
**	index	z0\.d, #0, #-16
**	ret
*/
TEST_S (index_u64_0_m16, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, -16))

/*
** index_u64_0_1:
**	index	z0\.d, #0, #1
**	ret
*/
TEST_S (index_u64_0_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, 1))

/*
** index_u64_0_15:
**	index	z0\.d, #0, #15
**	ret
*/
TEST_S (index_u64_0_15, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, 15))

/*
** index_u64_0_16:
**	mov	(x[0-9]+), 16
**	index	z0\.d, #0, \1
**	ret
*/
TEST_S (index_u64_0_16, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, 16))

/*
** index_u64_m17_1:
**	mov	(x[0-9]+), -17
**	index	z0\.d, \1, #1
**	ret
*/
TEST_S (index_u64_m17_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (-17, 1))

/*
** index_u64_m16_1:
**	index	z0\.d, #-16, #1
**	ret
*/
TEST_S (index_u64_m16_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (-16, 1))

/*
** index_u64_m1_1:
**	index	z0\.d, #-1, #1
**	ret
*/
TEST_S (index_u64_m1_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (-1, 1))

/*
** index_u64_1_1:
**	index	z0\.d, #1, #1
**	ret
*/
TEST_S (index_u64_1_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (1, 1))

/*
** index_u64_15_1:
**	index	z0\.d, #15, #1
**	ret
*/
TEST_S (index_u64_15_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (15, 1))

/*
** index_u64_16_1:
**	mov	(x[0-9]+), 16
**	index	z0\.d, \1, #1
**	ret
*/
TEST_S (index_u64_16_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (16, 1))

/*
** index_u64_m17_x0:
**	mov	(x[0-9]+), -17
**	index	z0\.d, \1, x0
**	ret
*/
TEST_S (index_u64_m17_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (-17, x0))

/*
** index_u64_m16_x0:
**	index	z0\.d, #-16, x0
**	ret
*/
TEST_S (index_u64_m16_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (-16, x0))

/*
** index_u64_m1_x0:
**	index	z0\.d, #-1, x0
**	ret
*/
TEST_S (index_u64_m1_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (-1, x0))

/*
** index_u64_0_x0:
**	index	z0\.d, #0, x0
**	ret
*/
TEST_S (index_u64_0_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (0, x0))

/*
** index_u64_1_x0:
**	index	z0\.d, #1, x0
**	ret
*/
TEST_S (index_u64_1_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (1, x0))

/*
** index_u64_15_x0:
**	index	z0\.d, #15, x0
**	ret
*/
TEST_S (index_u64_15_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (15, x0))

/*
** index_u64_16_x0:
**	mov	(x[0-9]+), 16
**	index	z0\.d, \1, x0
**	ret
*/
TEST_S (index_u64_16_x0, svuint64_t, uint64_t,
	z0 = svindex_u64 (16, x0))

/*
** index_u64_x0_m17:
**	mov	(x[0-9]+), -17
**	index	z0\.d, x0, \1
**	ret
*/
TEST_S (index_u64_x0_m17, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, -17))

/*
** index_u64_x0_m16:
**	index	z0\.d, x0, #-16
**	ret
*/
TEST_S (index_u64_x0_m16, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, -16))

/*
** index_u64_x0_1:
**	index	z0\.d, x0, #1
**	ret
*/
TEST_S (index_u64_x0_1, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, 1))

/*
** index_u64_x0_15:
**	index	z0\.d, x0, #15
**	ret
*/
TEST_S (index_u64_x0_15, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, 15))

/*
** index_u64_x0_16:
**	mov	(x[0-9]+), 16
**	index	z0\.d, x0, \1
**	ret
*/
TEST_S (index_u64_x0_16, svuint64_t, uint64_t,
	z0 = svindex_u64 (x0, 16))
