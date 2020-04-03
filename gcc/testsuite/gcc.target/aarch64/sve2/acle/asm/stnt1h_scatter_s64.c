/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1h_scatter_s64:
**	stnt1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_s64, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_s64 (p0, z1, z0),
		       svstnt1h_scatter (p0, z1, z0))

/*
** stnt1h_scatter_x0_s64_offset:
**	stnt1h	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_x0_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, x0, z0),
		       svstnt1h_scatter_offset (p0, z1, x0, z0))

/*
** stnt1h_scatter_m2_s64_offset:
**	mov	(x[0-9]+), #?-2
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_m2_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, -2, z0),
		       svstnt1h_scatter_offset (p0, z1, -2, z0))

/*
** stnt1h_scatter_0_s64_offset:
**	stnt1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_0_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, 0, z0),
		       svstnt1h_scatter_offset (p0, z1, 0, z0))

/*
** stnt1h_scatter_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_5_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, 5, z0),
		       svstnt1h_scatter_offset (p0, z1, 5, z0))

/*
** stnt1h_scatter_6_s64_offset:
**	mov	(x[0-9]+), #?6
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_6_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, 6, z0),
		       svstnt1h_scatter_offset (p0, z1, 6, z0))

/*
** stnt1h_scatter_62_s64_offset:
**	mov	(x[0-9]+), #?62
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_62_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, 62, z0),
		       svstnt1h_scatter_offset (p0, z1, 62, z0))

/*
** stnt1h_scatter_64_s64_offset:
**	mov	(x[0-9]+), #?64
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_64_s64_offset, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_offset_s64 (p0, z1, 64, z0),
		       svstnt1h_scatter_offset (p0, z1, 64, z0))

/*
** stnt1h_scatter_x0_s64_index:
**	lsl	(x[0-9]+), x0, #?1
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_x0_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, x0, z0),
		       svstnt1h_scatter_index (p0, z1, x0, z0))

/*
** stnt1h_scatter_m1_s64_index:
**	mov	(x[0-9]+), #?-2
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_m1_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, -1, z0),
		       svstnt1h_scatter_index (p0, z1, -1, z0))

/*
** stnt1h_scatter_0_s64_index:
**	stnt1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_0_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, 0, z0),
		       svstnt1h_scatter_index (p0, z1, 0, z0))

/*
** stnt1h_scatter_5_s64_index:
**	mov	(x[0-9]+), #?10
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_5_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, 5, z0),
		       svstnt1h_scatter_index (p0, z1, 5, z0))

/*
** stnt1h_scatter_31_s64_index:
**	mov	(x[0-9]+), #?62
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_31_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, 31, z0),
		       svstnt1h_scatter_index (p0, z1, 31, z0))

/*
** stnt1h_scatter_32_s64_index:
**	mov	(x[0-9]+), #?64
**	stnt1h	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_32_s64_index, svint64_t, svuint64_t,
		       svstnt1h_scatter_u64base_index_s64 (p0, z1, 32, z0),
		       svstnt1h_scatter_index (p0, z1, 32, z0))

/*
** stnt1h_scatter_x0_s64_s64offset:
**	stnt1h	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_x0_s64_s64offset, svint64_t, int16_t, svint64_t,
		       svstnt1h_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))

/*
** stnt1h_scatter_s64_s64offset:
**	stnt1h	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_s64_s64offset, svint64_t, int16_t, svint64_t,
		       svstnt1h_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))

/*
** stnt1h_scatter_x0_s64_u64offset:
**	stnt1h	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_x0_s64_u64offset, svint64_t, int16_t, svuint64_t,
		       svstnt1h_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))

/*
** stnt1h_scatter_s64_u64offset:
**	stnt1h	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_s64_u64offset, svint64_t, int16_t, svuint64_t,
		       svstnt1h_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))

/*
** stnt1h_scatter_x0_s64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	stnt1h	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_x0_s64_s64index, svint64_t, int16_t, svint64_t,
		       svstnt1h_scatter_s64index_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_index (p0, x0, z1, z0))

/*
** stnt1h_scatter_s64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	stnt1h	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_s64_s64index, svint64_t, int16_t, svint64_t,
		       svstnt1h_scatter_s64index_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_index (p0, x0, z1, z0))

/*
** stnt1h_scatter_x0_s64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	stnt1h	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_x0_s64_u64index, svint64_t, int16_t, svuint64_t,
		       svstnt1h_scatter_u64index_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_index (p0, x0, z1, z0))

/*
** stnt1h_scatter_s64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #1
**	stnt1h	z0\.d, p0, \[\1, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_s64_u64index, svint64_t, int16_t, svuint64_t,
		       svstnt1h_scatter_u64index_s64 (p0, x0, z1, z0),
		       svstnt1h_scatter_index (p0, x0, z1, z0))
