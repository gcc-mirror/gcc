/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1_scatter_u64:
**	st1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_u64, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_u64 (p0, z1, z0),
		       svst1_scatter (p0, z1, z0))

/*
** st1_scatter_x0_u64_offset:
**	st1d	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, x0, z0),
		       svst1_scatter_offset (p0, z1, x0, z0))

/*
** st1_scatter_m8_u64_offset:
**	mov	(x[0-9]+), #?-8
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m8_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, -8, z0),
		       svst1_scatter_offset (p0, z1, -8, z0))

/*
** st1_scatter_0_u64_offset:
**	st1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 0, z0),
		       svst1_scatter_offset (p0, z1, 0, z0))

/*
** st1_scatter_9_u64_offset:
**	mov	(x[0-9]+), #?9
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_9_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 9, z0),
		       svst1_scatter_offset (p0, z1, 9, z0))

/*
** st1_scatter_10_u64_offset:
**	mov	(x[0-9]+), #?10
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_10_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 10, z0),
		       svst1_scatter_offset (p0, z1, 10, z0))

/*
** st1_scatter_11_u64_offset:
**	mov	(x[0-9]+), #?11
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_11_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 11, z0),
		       svst1_scatter_offset (p0, z1, 11, z0))

/*
** st1_scatter_12_u64_offset:
**	mov	(x[0-9]+), #?12
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_12_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 12, z0),
		       svst1_scatter_offset (p0, z1, 12, z0))

/*
** st1_scatter_13_u64_offset:
**	mov	(x[0-9]+), #?13
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_13_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 13, z0),
		       svst1_scatter_offset (p0, z1, 13, z0))

/*
** st1_scatter_14_u64_offset:
**	mov	(x[0-9]+), #?14
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_14_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 14, z0),
		       svst1_scatter_offset (p0, z1, 14, z0))

/*
** st1_scatter_15_u64_offset:
**	mov	(x[0-9]+), #?15
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_15_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 15, z0),
		       svst1_scatter_offset (p0, z1, 15, z0))

/*
** st1_scatter_16_u64_offset:
**	st1d	z0\.d, p0, \[z1\.d, #16\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_16_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 16, z0),
		       svst1_scatter_offset (p0, z1, 16, z0))

/*
** st1_scatter_248_u64_offset:
**	st1d	z0\.d, p0, \[z1\.d, #248\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_248_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 248, z0),
		       svst1_scatter_offset (p0, z1, 248, z0))

/*
** st1_scatter_256_u64_offset:
**	mov	(x[0-9]+), #?256
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_256_u64_offset, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_offset_u64 (p0, z1, 256, z0),
		       svst1_scatter_offset (p0, z1, 256, z0))

/*
** st1_scatter_x0_u64_index:
**	lsl	(x[0-9]+), x0, #?3
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, x0, z0),
		       svst1_scatter_index (p0, z1, x0, z0))

/*
** st1_scatter_m1_u64_index:
**	mov	(x[0-9]+), #?-8
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m1_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, -1, z0),
		       svst1_scatter_index (p0, z1, -1, z0))

/*
** st1_scatter_0_u64_index:
**	st1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, 0, z0),
		       svst1_scatter_index (p0, z1, 0, z0))

/*
** st1_scatter_5_u64_index:
**	st1d	z0\.d, p0, \[z1\.d, #40\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_5_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, 5, z0),
		       svst1_scatter_index (p0, z1, 5, z0))

/*
** st1_scatter_31_u64_index:
**	st1d	z0\.d, p0, \[z1\.d, #248\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_31_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, 31, z0),
		       svst1_scatter_index (p0, z1, 31, z0))

/*
** st1_scatter_32_u64_index:
**	mov	(x[0-9]+), #?256
**	st1d	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_32_u64_index, svuint64_t, svuint64_t,
		       svst1_scatter_u64base_index_u64 (p0, z1, 32, z0),
		       svst1_scatter_index (p0, z1, 32, z0))

/*
** st1_scatter_x0_u64_s64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_u64_s64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_ext_u64_s64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_ext_u64_s64offset, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64offset_u64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1_scatter_x0_u64_u64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_u64_u64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_ext_u64_u64offset:
**	st1d	z0\.d, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_ext_u64_u64offset, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64offset_u64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1_scatter_x0_u64_s64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u64_s64index, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64index_u64 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_u64_s64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u64_s64index, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64index_u64 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_ext_u64_s64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, sxtw 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_ext_u64_s64index, svuint64_t, uint64_t, svint64_t,
		       svst1_scatter_s64index_u64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1_scatter_index (p0, x0, svextw_x (p0, z1), z0))

/*
** st1_scatter_x0_u64_u64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64index_u64 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_u64_u64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64index_u64 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_ext_u64_u64index:
**	st1d	z0\.d, p0, \[x0, z1\.d, uxtw 3\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_ext_u64_u64index, svuint64_t, uint64_t, svuint64_t,
		       svst1_scatter_u64index_u64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1_scatter_index (p0, x0, svextw_x (p0, z1), z0))
