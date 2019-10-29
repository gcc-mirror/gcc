/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1h_scatter_s64:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_s64, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_s64 (p0, z1, z0),
		       svst1h_scatter (p0, z1, z0))

/*
** st1h_scatter_x0_s64_offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, x0, z0),
		       svst1h_scatter_offset (p0, z1, x0, z0))

/*
** st1h_scatter_m2_s64_offset:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m2_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, -2, z0),
		       svst1h_scatter_offset (p0, z1, -2, z0))

/*
** st1h_scatter_0_s64_offset:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, 0, z0),
		       svst1h_scatter_offset (p0, z1, 0, z0))

/*
** st1h_scatter_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, 5, z0),
		       svst1h_scatter_offset (p0, z1, 5, z0))

/*
** st1h_scatter_6_s64_offset:
**	st1h	z0\.d, p0, \[z1\.d, #6\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_6_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, 6, z0),
		       svst1h_scatter_offset (p0, z1, 6, z0))

/*
** st1h_scatter_62_s64_offset:
**	st1h	z0\.d, p0, \[z1\.d, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_62_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, 62, z0),
		       svst1h_scatter_offset (p0, z1, 62, z0))

/*
** st1h_scatter_64_s64_offset:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_64_s64_offset, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_s64 (p0, z1, 64, z0),
		       svst1h_scatter_offset (p0, z1, 64, z0))

/*
** st1h_scatter_x0_s64_index:
**	lsl	(x[0-9]+), x0, #?1
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, x0, z0),
		       svst1h_scatter_index (p0, z1, x0, z0))

/*
** st1h_scatter_m1_s64_index:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m1_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, -1, z0),
		       svst1h_scatter_index (p0, z1, -1, z0))

/*
** st1h_scatter_0_s64_index:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, 0, z0),
		       svst1h_scatter_index (p0, z1, 0, z0))

/*
** st1h_scatter_5_s64_index:
**	st1h	z0\.d, p0, \[z1\.d, #10\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, 5, z0),
		       svst1h_scatter_index (p0, z1, 5, z0))

/*
** st1h_scatter_31_s64_index:
**	st1h	z0\.d, p0, \[z1\.d, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_31_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, 31, z0),
		       svst1h_scatter_index (p0, z1, 31, z0))

/*
** st1h_scatter_32_s64_index:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_32_s64_index, svint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_s64 (p0, z1, 32, z0),
		       svst1h_scatter_index (p0, z1, 32, z0))

/*
** st1h_scatter_x0_s64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_s64_s64offset, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_s64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_s64_s64offset, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_ext_s64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_s64_s64offset, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64offset_s64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1h_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_s64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_s64_u64offset, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_s64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_s64_u64offset, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_ext_s64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_s64_u64offset, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64offset_s64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1h_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_s64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_s64_s64index, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64index_s64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_s64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_s64_s64index, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64index_s64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_ext_s64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, sxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_s64_s64index, svint64_t, int16_t, svint64_t,
		       svst1h_scatter_s64index_s64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1h_scatter_index (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_s64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_s64_u64index, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64index_s64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_s64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_s64_u64index, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64index_s64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_ext_s64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, uxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_s64_u64index, svint64_t, int16_t, svuint64_t,
		       svst1h_scatter_u64index_s64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1h_scatter_index (p0, x0, svextw_x (p0, z1), z0))
