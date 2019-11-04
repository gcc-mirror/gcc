/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** index_s64_x0_x1:
**	index	z0\.d, x0, x1
**	ret
*/
TEST_S (index_s64_x0_x1, svint64_t, int64_t,
	z0 = svindex_s64 (x0, x1))

/*
** index_s64_x0_2:
**	index	z0\.d, x0, #2
**	ret
*/
TEST_S (index_s64_x0_2, svint64_t, int64_t,
	z0 = svindex_s64 (x0, 2))

/*
** index_s64_50_2:
**	mov	(x[0-9]+), 50
**	index	z0\.d, \1, #2
**	ret
*/
TEST_S (index_s64_50_2, svint64_t, int64_t,
	z0 = svindex_s64 (50, 2))

/*
** index_s64_0_m17:
**	mov	(x[0-9]+), -17
**	index	z0\.d, #0, \1
**	ret
*/
TEST_S (index_s64_0_m17, svint64_t, int64_t,
	z0 = svindex_s64 (0, -17))

/*
** index_s64_0_m16:
**	index	z0\.d, #0, #-16
**	ret
*/
TEST_S (index_s64_0_m16, svint64_t, int64_t,
	z0 = svindex_s64 (0, -16))

/*
** index_s64_0_1:
**	index	z0\.d, #0, #1
**	ret
*/
TEST_S (index_s64_0_1, svint64_t, int64_t,
	z0 = svindex_s64 (0, 1))

/*
** index_s64_0_15:
**	index	z0\.d, #0, #15
**	ret
*/
TEST_S (index_s64_0_15, svint64_t, int64_t,
	z0 = svindex_s64 (0, 15))

/*
** index_s64_0_16:
**	mov	(x[0-9]+), 16
**	index	z0\.d, #0, \1
**	ret
*/
TEST_S (index_s64_0_16, svint64_t, int64_t,
	z0 = svindex_s64 (0, 16))

/*
** index_s64_m17_1:
**	mov	(x[0-9]+), -17
**	index	z0\.d, \1, #1
**	ret
*/
TEST_S (index_s64_m17_1, svint64_t, int64_t,
	z0 = svindex_s64 (-17, 1))

/*
** index_s64_m16_1:
**	index	z0\.d, #-16, #1
**	ret
*/
TEST_S (index_s64_m16_1, svint64_t, int64_t,
	z0 = svindex_s64 (-16, 1))

/*
** index_s64_m1_1:
**	index	z0\.d, #-1, #1
**	ret
*/
TEST_S (index_s64_m1_1, svint64_t, int64_t,
	z0 = svindex_s64 (-1, 1))

/*
** index_s64_1_1:
**	index	z0\.d, #1, #1
**	ret
*/
TEST_S (index_s64_1_1, svint64_t, int64_t,
	z0 = svindex_s64 (1, 1))

/*
** index_s64_15_1:
**	index	z0\.d, #15, #1
**	ret
*/
TEST_S (index_s64_15_1, svint64_t, int64_t,
	z0 = svindex_s64 (15, 1))

/*
** index_s64_16_1:
**	mov	(x[0-9]+), 16
**	index	z0\.d, \1, #1
**	ret
*/
TEST_S (index_s64_16_1, svint64_t, int64_t,
	z0 = svindex_s64 (16, 1))

/*
** index_s64_m17_x0:
**	mov	(x[0-9]+), -17
**	index	z0\.d, \1, x0
**	ret
*/
TEST_S (index_s64_m17_x0, svint64_t, int64_t,
	z0 = svindex_s64 (-17, x0))

/*
** index_s64_m16_x0:
**	index	z0\.d, #-16, x0
**	ret
*/
TEST_S (index_s64_m16_x0, svint64_t, int64_t,
	z0 = svindex_s64 (-16, x0))

/*
** index_s64_m1_x0:
**	index	z0\.d, #-1, x0
**	ret
*/
TEST_S (index_s64_m1_x0, svint64_t, int64_t,
	z0 = svindex_s64 (-1, x0))

/*
** index_s64_0_x0:
**	index	z0\.d, #0, x0
**	ret
*/
TEST_S (index_s64_0_x0, svint64_t, int64_t,
	z0 = svindex_s64 (0, x0))

/*
** index_s64_1_x0:
**	index	z0\.d, #1, x0
**	ret
*/
TEST_S (index_s64_1_x0, svint64_t, int64_t,
	z0 = svindex_s64 (1, x0))

/*
** index_s64_15_x0:
**	index	z0\.d, #15, x0
**	ret
*/
TEST_S (index_s64_15_x0, svint64_t, int64_t,
	z0 = svindex_s64 (15, x0))

/*
** index_s64_16_x0:
**	mov	(x[0-9]+), 16
**	index	z0\.d, \1, x0
**	ret
*/
TEST_S (index_s64_16_x0, svint64_t, int64_t,
	z0 = svindex_s64 (16, x0))

/*
** index_s64_x0_m17:
**	mov	(x[0-9]+), -17
**	index	z0\.d, x0, \1
**	ret
*/
TEST_S (index_s64_x0_m17, svint64_t, int64_t,
	z0 = svindex_s64 (x0, -17))

/*
** index_s64_x0_m16:
**	index	z0\.d, x0, #-16
**	ret
*/
TEST_S (index_s64_x0_m16, svint64_t, int64_t,
	z0 = svindex_s64 (x0, -16))

/*
** index_s64_x0_1:
**	index	z0\.d, x0, #1
**	ret
*/
TEST_S (index_s64_x0_1, svint64_t, int64_t,
	z0 = svindex_s64 (x0, 1))

/*
** index_s64_x0_15:
**	index	z0\.d, x0, #15
**	ret
*/
TEST_S (index_s64_x0_15, svint64_t, int64_t,
	z0 = svindex_s64 (x0, 15))

/*
** index_s64_x0_16:
**	mov	(x[0-9]+), 16
**	index	z0\.d, x0, \1
**	ret
*/
TEST_S (index_s64_x0_16, svint64_t, int64_t,
	z0 = svindex_s64 (x0, 16))
