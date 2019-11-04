/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** index_s32_w0_w1:
**	index	z0\.s, w0, w1
**	ret
*/
TEST_S (index_s32_w0_w1, svint32_t, int32_t,
	z0 = svindex_s32 (x0, x1))

/*
** index_s32_w0_2:
**	index	z0\.s, w0, #2
**	ret
*/
TEST_S (index_s32_w0_2, svint32_t, int32_t,
	z0 = svindex_s32 (x0, 2))

/*
** index_s32_50_2:
**	mov	(w[0-9]+), 50
**	index	z0\.s, \1, #2
**	ret
*/
TEST_S (index_s32_50_2, svint32_t, int32_t,
	z0 = svindex_s32 (50, 2))

/*
** index_s32_0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.s, #0, \1
**	ret
*/
TEST_S (index_s32_0_m17, svint32_t, int32_t,
	z0 = svindex_s32 (0, -17))

/*
** index_s32_0_m16:
**	index	z0\.s, #0, #-16
**	ret
*/
TEST_S (index_s32_0_m16, svint32_t, int32_t,
	z0 = svindex_s32 (0, -16))

/*
** index_s32_0_1:
**	index	z0\.s, #0, #1
**	ret
*/
TEST_S (index_s32_0_1, svint32_t, int32_t,
	z0 = svindex_s32 (0, 1))

/*
** index_s32_0_15:
**	index	z0\.s, #0, #15
**	ret
*/
TEST_S (index_s32_0_15, svint32_t, int32_t,
	z0 = svindex_s32 (0, 15))

/*
** index_s32_0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.s, #0, \1
**	ret
*/
TEST_S (index_s32_0_16, svint32_t, int32_t,
	z0 = svindex_s32 (0, 16))

/*
** index_s32_m17_1:
**	mov	(w[0-9]+), -17
**	index	z0\.s, \1, #1
**	ret
*/
TEST_S (index_s32_m17_1, svint32_t, int32_t,
	z0 = svindex_s32 (-17, 1))

/*
** index_s32_m16_1:
**	index	z0\.s, #-16, #1
**	ret
*/
TEST_S (index_s32_m16_1, svint32_t, int32_t,
	z0 = svindex_s32 (-16, 1))

/*
** index_s32_m1_1:
**	index	z0\.s, #-1, #1
**	ret
*/
TEST_S (index_s32_m1_1, svint32_t, int32_t,
	z0 = svindex_s32 (-1, 1))

/*
** index_s32_1_1:
**	index	z0\.s, #1, #1
**	ret
*/
TEST_S (index_s32_1_1, svint32_t, int32_t,
	z0 = svindex_s32 (1, 1))

/*
** index_s32_15_1:
**	index	z0\.s, #15, #1
**	ret
*/
TEST_S (index_s32_15_1, svint32_t, int32_t,
	z0 = svindex_s32 (15, 1))

/*
** index_s32_16_1:
**	mov	(w[0-9]+), 16
**	index	z0\.s, \1, #1
**	ret
*/
TEST_S (index_s32_16_1, svint32_t, int32_t,
	z0 = svindex_s32 (16, 1))

/*
** index_s32_m17_x0:
**	mov	(w[0-9]+), -17
**	index	z0\.s, \1, w0
**	ret
*/
TEST_S (index_s32_m17_x0, svint32_t, int32_t,
	z0 = svindex_s32 (-17, x0))

/*
** index_s32_m16_x0:
**	index	z0\.s, #-16, w0
**	ret
*/
TEST_S (index_s32_m16_x0, svint32_t, int32_t,
	z0 = svindex_s32 (-16, x0))

/*
** index_s32_m1_x0:
**	index	z0\.s, #-1, w0
**	ret
*/
TEST_S (index_s32_m1_x0, svint32_t, int32_t,
	z0 = svindex_s32 (-1, x0))

/*
** index_s32_0_x0:
**	index	z0\.s, #0, w0
**	ret
*/
TEST_S (index_s32_0_x0, svint32_t, int32_t,
	z0 = svindex_s32 (0, x0))

/*
** index_s32_1_x0:
**	index	z0\.s, #1, w0
**	ret
*/
TEST_S (index_s32_1_x0, svint32_t, int32_t,
	z0 = svindex_s32 (1, x0))

/*
** index_s32_15_x0:
**	index	z0\.s, #15, w0
**	ret
*/
TEST_S (index_s32_15_x0, svint32_t, int32_t,
	z0 = svindex_s32 (15, x0))

/*
** index_s32_16_x0:
**	mov	(w[0-9]+), 16
**	index	z0\.s, \1, w0
**	ret
*/
TEST_S (index_s32_16_x0, svint32_t, int32_t,
	z0 = svindex_s32 (16, x0))

/*
** index_s32_x0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.s, w0, \1
**	ret
*/
TEST_S (index_s32_x0_m17, svint32_t, int32_t,
	z0 = svindex_s32 (x0, -17))

/*
** index_s32_x0_m16:
**	index	z0\.s, w0, #-16
**	ret
*/
TEST_S (index_s32_x0_m16, svint32_t, int32_t,
	z0 = svindex_s32 (x0, -16))

/*
** index_s32_x0_1:
**	index	z0\.s, w0, #1
**	ret
*/
TEST_S (index_s32_x0_1, svint32_t, int32_t,
	z0 = svindex_s32 (x0, 1))

/*
** index_s32_x0_15:
**	index	z0\.s, w0, #15
**	ret
*/
TEST_S (index_s32_x0_15, svint32_t, int32_t,
	z0 = svindex_s32 (x0, 15))

/*
** index_s32_x0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.s, w0, \1
**	ret
*/
TEST_S (index_s32_x0_16, svint32_t, int32_t,
	z0 = svindex_s32 (x0, 16))
