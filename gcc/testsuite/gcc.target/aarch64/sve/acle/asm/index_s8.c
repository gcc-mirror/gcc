/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** index_s8_w0_w1:
**	index	z0\.b, w0, w1
**	ret
*/
TEST_S (index_s8_w0_w1, svint8_t, int8_t,
	z0 = svindex_s8 (x0, x1))

/*
** index_s8_w0_2:
**	index	z0\.b, w0, #2
**	ret
*/
TEST_S (index_s8_w0_2, svint8_t, int8_t,
	z0 = svindex_s8 (x0, 2))

/*
** index_s8_50_2:
**	mov	(w[0-9]+), 50
**	index	z0\.b, \1, #2
**	ret
*/
TEST_S (index_s8_50_2, svint8_t, int8_t,
	z0 = svindex_s8 (50, 2))

/*
** index_s8_0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.b, #0, \1
**	ret
*/
TEST_S (index_s8_0_m17, svint8_t, int8_t,
	z0 = svindex_s8 (0, -17))

/*
** index_s8_0_m16:
**	index	z0\.b, #0, #-16
**	ret
*/
TEST_S (index_s8_0_m16, svint8_t, int8_t,
	z0 = svindex_s8 (0, -16))

/*
** index_s8_0_1:
**	index	z0\.b, #0, #1
**	ret
*/
TEST_S (index_s8_0_1, svint8_t, int8_t,
	z0 = svindex_s8 (0, 1))

/*
** index_s8_0_15:
**	index	z0\.b, #0, #15
**	ret
*/
TEST_S (index_s8_0_15, svint8_t, int8_t,
	z0 = svindex_s8 (0, 15))

/*
** index_s8_0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.b, #0, \1
**	ret
*/
TEST_S (index_s8_0_16, svint8_t, int8_t,
	z0 = svindex_s8 (0, 16))

/*
** index_s8_m17_1:
**	mov	(w[0-9]+), -17
**	index	z0\.b, \1, #1
**	ret
*/
TEST_S (index_s8_m17_1, svint8_t, int8_t,
	z0 = svindex_s8 (-17, 1))

/*
** index_s8_m16_1:
**	index	z0\.b, #-16, #1
**	ret
*/
TEST_S (index_s8_m16_1, svint8_t, int8_t,
	z0 = svindex_s8 (-16, 1))

/*
** index_s8_m1_1:
**	index	z0\.b, #-1, #1
**	ret
*/
TEST_S (index_s8_m1_1, svint8_t, int8_t,
	z0 = svindex_s8 (-1, 1))

/*
** index_s8_1_1:
**	index	z0\.b, #1, #1
**	ret
*/
TEST_S (index_s8_1_1, svint8_t, int8_t,
	z0 = svindex_s8 (1, 1))

/*
** index_s8_15_1:
**	index	z0\.b, #15, #1
**	ret
*/
TEST_S (index_s8_15_1, svint8_t, int8_t,
	z0 = svindex_s8 (15, 1))

/*
** index_s8_16_1:
**	mov	(w[0-9]+), 16
**	index	z0\.b, \1, #1
**	ret
*/
TEST_S (index_s8_16_1, svint8_t, int8_t,
	z0 = svindex_s8 (16, 1))

/*
** index_s8_m17_x0:
**	mov	(w[0-9]+), -17
**	index	z0\.b, \1, w0
**	ret
*/
TEST_S (index_s8_m17_x0, svint8_t, int8_t,
	z0 = svindex_s8 (-17, x0))

/*
** index_s8_m16_x0:
**	index	z0\.b, #-16, w0
**	ret
*/
TEST_S (index_s8_m16_x0, svint8_t, int8_t,
	z0 = svindex_s8 (-16, x0))

/*
** index_s8_m1_x0:
**	index	z0\.b, #-1, w0
**	ret
*/
TEST_S (index_s8_m1_x0, svint8_t, int8_t,
	z0 = svindex_s8 (-1, x0))

/*
** index_s8_0_x0:
**	index	z0\.b, #0, w0
**	ret
*/
TEST_S (index_s8_0_x0, svint8_t, int8_t,
	z0 = svindex_s8 (0, x0))

/*
** index_s8_1_x0:
**	index	z0\.b, #1, w0
**	ret
*/
TEST_S (index_s8_1_x0, svint8_t, int8_t,
	z0 = svindex_s8 (1, x0))

/*
** index_s8_15_x0:
**	index	z0\.b, #15, w0
**	ret
*/
TEST_S (index_s8_15_x0, svint8_t, int8_t,
	z0 = svindex_s8 (15, x0))

/*
** index_s8_16_x0:
**	mov	(w[0-9]+), 16
**	index	z0\.b, \1, w0
**	ret
*/
TEST_S (index_s8_16_x0, svint8_t, int8_t,
	z0 = svindex_s8 (16, x0))

/*
** index_s8_x0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.b, w0, \1
**	ret
*/
TEST_S (index_s8_x0_m17, svint8_t, int8_t,
	z0 = svindex_s8 (x0, -17))

/*
** index_s8_x0_m16:
**	index	z0\.b, w0, #-16
**	ret
*/
TEST_S (index_s8_x0_m16, svint8_t, int8_t,
	z0 = svindex_s8 (x0, -16))

/*
** index_s8_x0_1:
**	index	z0\.b, w0, #1
**	ret
*/
TEST_S (index_s8_x0_1, svint8_t, int8_t,
	z0 = svindex_s8 (x0, 1))

/*
** index_s8_x0_15:
**	index	z0\.b, w0, #15
**	ret
*/
TEST_S (index_s8_x0_15, svint8_t, int8_t,
	z0 = svindex_s8 (x0, 15))

/*
** index_s8_x0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.b, w0, \1
**	ret
*/
TEST_S (index_s8_x0_16, svint8_t, int8_t,
	z0 = svindex_s8 (x0, 16))
