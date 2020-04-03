/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1_scatter_s64:
**	stnt1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_s64, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_s64 (p0, z1, z0),
		       svstnt1_scatter (p0, z1, z0))

/*
** stnt1_scatter_x0_s64_offset:
**	stnt1d	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_x0_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, x0, z0),
		       svstnt1_scatter_offset (p0, z1, x0, z0))

/*
** stnt1_scatter_m8_s64_offset:
**	mov	(x[0-9]+), #?-8
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_m8_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, -8, z0),
		       svstnt1_scatter_offset (p0, z1, -8, z0))

/*
** stnt1_scatter_0_s64_offset:
**	stnt1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_0_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 0, z0),
		       svstnt1_scatter_offset (p0, z1, 0, z0))

/*
** stnt1_scatter_9_s64_offset:
**	mov	(x[0-9]+), #?9
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_9_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 9, z0),
		       svstnt1_scatter_offset (p0, z1, 9, z0))

/*
** stnt1_scatter_10_s64_offset:
**	mov	(x[0-9]+), #?10
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_10_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 10, z0),
		       svstnt1_scatter_offset (p0, z1, 10, z0))

/*
** stnt1_scatter_11_s64_offset:
**	mov	(x[0-9]+), #?11
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_11_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 11, z0),
		       svstnt1_scatter_offset (p0, z1, 11, z0))

/*
** stnt1_scatter_12_s64_offset:
**	mov	(x[0-9]+), #?12
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_12_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 12, z0),
		       svstnt1_scatter_offset (p0, z1, 12, z0))

/*
** stnt1_scatter_13_s64_offset:
**	mov	(x[0-9]+), #?13
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_13_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 13, z0),
		       svstnt1_scatter_offset (p0, z1, 13, z0))

/*
** stnt1_scatter_14_s64_offset:
**	mov	(x[0-9]+), #?14
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_14_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 14, z0),
		       svstnt1_scatter_offset (p0, z1, 14, z0))

/*
** stnt1_scatter_15_s64_offset:
**	mov	(x[0-9]+), #?15
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_15_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 15, z0),
		       svstnt1_scatter_offset (p0, z1, 15, z0))

/*
** stnt1_scatter_16_s64_offset:
**	mov	(x[0-9]+), #?16
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_16_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 16, z0),
		       svstnt1_scatter_offset (p0, z1, 16, z0))

/*
** stnt1_scatter_248_s64_offset:
**	mov	(x[0-9]+), #?248
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_248_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 248, z0),
		       svstnt1_scatter_offset (p0, z1, 248, z0))

/*
** stnt1_scatter_256_s64_offset:
**	mov	(x[0-9]+), #?256
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_256_s64_offset, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_offset_s64 (p0, z1, 256, z0),
		       svstnt1_scatter_offset (p0, z1, 256, z0))

/*
** stnt1_scatter_x0_s64_index:
**	lsl	(x[0-9]+), x0, #?3
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_x0_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, x0, z0),
		       svstnt1_scatter_index (p0, z1, x0, z0))

/*
** stnt1_scatter_m1_s64_index:
**	mov	(x[0-9]+), #?-8
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_m1_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, -1, z0),
		       svstnt1_scatter_index (p0, z1, -1, z0))

/*
** stnt1_scatter_0_s64_index:
**	stnt1d	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_0_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, 0, z0),
		       svstnt1_scatter_index (p0, z1, 0, z0))

/*
** stnt1_scatter_5_s64_index:
**	mov	(x[0-9]+), #?40
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_5_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, 5, z0),
		       svstnt1_scatter_index (p0, z1, 5, z0))

/*
** stnt1_scatter_31_s64_index:
**	mov	(x[0-9]+), #?248
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_31_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, 31, z0),
		       svstnt1_scatter_index (p0, z1, 31, z0))

/*
** stnt1_scatter_32_s64_index:
**	mov	(x[0-9]+), #?256
**	stnt1d	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_32_s64_index, svint64_t, svuint64_t,
		       svstnt1_scatter_u64base_index_s64 (p0, z1, 32, z0),
		       svstnt1_scatter_index (p0, z1, 32, z0))

/*
** stnt1_scatter_x0_s64_s64offset:
**	stnt1d	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_x0_s64_s64offset, svint64_t, int64_t, svint64_t,
		       svstnt1_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))

/*
** stnt1_scatter_s64_s64offset:
**	stnt1d	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_s64_s64offset, svint64_t, int64_t, svint64_t,
		       svstnt1_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))

/*
** stnt1_scatter_x0_s64_u64offset:
**	stnt1d	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_x0_s64_u64offset, svint64_t, int64_t, svuint64_t,
		       svstnt1_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))

/*
** stnt1_scatter_s64_u64offset:
**	stnt1d	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_s64_u64offset, svint64_t, int64_t, svuint64_t,
		       svstnt1_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))

/*
** stnt1_scatter_x0_s64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	stnt1d	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_x0_s64_s64index, svint64_t, int64_t, svint64_t,
		       svstnt1_scatter_s64index_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_index (p0, x0, z1, z0))

/*
** stnt1_scatter_s64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	stnt1d	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_s64_s64index, svint64_t, int64_t, svint64_t,
		       svstnt1_scatter_s64index_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_index (p0, x0, z1, z0))

/*
** stnt1_scatter_x0_s64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	stnt1d	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_x0_s64_u64index, svint64_t, int64_t, svuint64_t,
		       svstnt1_scatter_u64index_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_index (p0, x0, z1, z0))

/*
** stnt1_scatter_s64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	stnt1d	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_s64_u64index, svint64_t, int64_t, svuint64_t,
		       svstnt1_scatter_u64index_s64 (p0, x0, z1, z0),
		       svstnt1_scatter_index (p0, x0, z1, z0))
