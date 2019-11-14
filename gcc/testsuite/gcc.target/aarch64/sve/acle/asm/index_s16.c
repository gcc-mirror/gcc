/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** index_s16_w0_w1:
**	index	z0\.h, w0, w1
**	ret
*/
TEST_S (index_s16_w0_w1, svint16_t, int16_t,
	z0 = svindex_s16 (x0, x1))

/*
** index_s16_w0_2:
**	index	z0\.h, w0, #2
**	ret
*/
TEST_S (index_s16_w0_2, svint16_t, int16_t,
	z0 = svindex_s16 (x0, 2))

/*
** index_s16_50_2:
**	mov	(w[0-9]+), 50
**	index	z0\.h, \1, #2
**	ret
*/
TEST_S (index_s16_50_2, svint16_t, int16_t,
	z0 = svindex_s16 (50, 2))

/*
** index_s16_0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.h, #0, \1
**	ret
*/
TEST_S (index_s16_0_m17, svint16_t, int16_t,
	z0 = svindex_s16 (0, -17))

/*
** index_s16_0_m16:
**	index	z0\.h, #0, #-16
**	ret
*/
TEST_S (index_s16_0_m16, svint16_t, int16_t,
	z0 = svindex_s16 (0, -16))

/*
** index_s16_0_1:
**	index	z0\.h, #0, #1
**	ret
*/
TEST_S (index_s16_0_1, svint16_t, int16_t,
	z0 = svindex_s16 (0, 1))

/*
** index_s16_0_15:
**	index	z0\.h, #0, #15
**	ret
*/
TEST_S (index_s16_0_15, svint16_t, int16_t,
	z0 = svindex_s16 (0, 15))

/*
** index_s16_0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.h, #0, \1
**	ret
*/
TEST_S (index_s16_0_16, svint16_t, int16_t,
	z0 = svindex_s16 (0, 16))

/*
** index_s16_m17_1:
**	mov	(w[0-9]+), -17
**	index	z0\.h, \1, #1
**	ret
*/
TEST_S (index_s16_m17_1, svint16_t, int16_t,
	z0 = svindex_s16 (-17, 1))

/*
** index_s16_m16_1:
**	index	z0\.h, #-16, #1
**	ret
*/
TEST_S (index_s16_m16_1, svint16_t, int16_t,
	z0 = svindex_s16 (-16, 1))

/*
** index_s16_m1_1:
**	index	z0\.h, #-1, #1
**	ret
*/
TEST_S (index_s16_m1_1, svint16_t, int16_t,
	z0 = svindex_s16 (-1, 1))

/*
** index_s16_1_1:
**	index	z0\.h, #1, #1
**	ret
*/
TEST_S (index_s16_1_1, svint16_t, int16_t,
	z0 = svindex_s16 (1, 1))

/*
** index_s16_15_1:
**	index	z0\.h, #15, #1
**	ret
*/
TEST_S (index_s16_15_1, svint16_t, int16_t,
	z0 = svindex_s16 (15, 1))

/*
** index_s16_16_1:
**	mov	(w[0-9]+), 16
**	index	z0\.h, \1, #1
**	ret
*/
TEST_S (index_s16_16_1, svint16_t, int16_t,
	z0 = svindex_s16 (16, 1))

/*
** index_s16_m17_x0:
**	mov	(w[0-9]+), -17
**	index	z0\.h, \1, w0
**	ret
*/
TEST_S (index_s16_m17_x0, svint16_t, int16_t,
	z0 = svindex_s16 (-17, x0))

/*
** index_s16_m16_x0:
**	index	z0\.h, #-16, w0
**	ret
*/
TEST_S (index_s16_m16_x0, svint16_t, int16_t,
	z0 = svindex_s16 (-16, x0))

/*
** index_s16_m1_x0:
**	index	z0\.h, #-1, w0
**	ret
*/
TEST_S (index_s16_m1_x0, svint16_t, int16_t,
	z0 = svindex_s16 (-1, x0))

/*
** index_s16_0_x0:
**	index	z0\.h, #0, w0
**	ret
*/
TEST_S (index_s16_0_x0, svint16_t, int16_t,
	z0 = svindex_s16 (0, x0))

/*
** index_s16_1_x0:
**	index	z0\.h, #1, w0
**	ret
*/
TEST_S (index_s16_1_x0, svint16_t, int16_t,
	z0 = svindex_s16 (1, x0))

/*
** index_s16_15_x0:
**	index	z0\.h, #15, w0
**	ret
*/
TEST_S (index_s16_15_x0, svint16_t, int16_t,
	z0 = svindex_s16 (15, x0))

/*
** index_s16_16_x0:
**	mov	(w[0-9]+), 16
**	index	z0\.h, \1, w0
**	ret
*/
TEST_S (index_s16_16_x0, svint16_t, int16_t,
	z0 = svindex_s16 (16, x0))

/*
** index_s16_x0_m17:
**	mov	(w[0-9]+), -17
**	index	z0\.h, w0, \1
**	ret
*/
TEST_S (index_s16_x0_m17, svint16_t, int16_t,
	z0 = svindex_s16 (x0, -17))

/*
** index_s16_x0_m16:
**	index	z0\.h, w0, #-16
**	ret
*/
TEST_S (index_s16_x0_m16, svint16_t, int16_t,
	z0 = svindex_s16 (x0, -16))

/*
** index_s16_x0_1:
**	index	z0\.h, w0, #1
**	ret
*/
TEST_S (index_s16_x0_1, svint16_t, int16_t,
	z0 = svindex_s16 (x0, 1))

/*
** index_s16_x0_15:
**	index	z0\.h, w0, #15
**	ret
*/
TEST_S (index_s16_x0_15, svint16_t, int16_t,
	z0 = svindex_s16 (x0, 15))

/*
** index_s16_x0_16:
**	mov	(w[0-9]+), 16
**	index	z0\.h, w0, \1
**	ret
*/
TEST_S (index_s16_x0_16, svint16_t, int16_t,
	z0 = svindex_s16 (x0, 16))
