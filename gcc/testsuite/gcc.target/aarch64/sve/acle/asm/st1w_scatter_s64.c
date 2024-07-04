/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st1w_scatter_s64:
**	st1w	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_s64, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_s64 (p0, z1, z0),
		       svst1w_scatter (p0, z1, z0))

/*
** st1w_scatter_x0_s64_offset:
**	st1w	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_x0_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, x0, z0),
		       svst1w_scatter_offset (p0, z1, x0, z0))

/*
** st1w_scatter_m4_s64_offset:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_m4_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, -4, z0),
		       svst1w_scatter_offset (p0, z1, -4, z0))

/*
** st1w_scatter_0_s64_offset:
**	st1w	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_0_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 0, z0),
		       svst1w_scatter_offset (p0, z1, 0, z0))

/*
** st1w_scatter_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_5_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 5, z0),
		       svst1w_scatter_offset (p0, z1, 5, z0))

/*
** st1w_scatter_6_s64_offset:
**	mov	(x[0-9]+), #?6
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_6_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 6, z0),
		       svst1w_scatter_offset (p0, z1, 6, z0))

/*
** st1w_scatter_7_s64_offset:
**	mov	(x[0-9]+), #?7
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_7_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 7, z0),
		       svst1w_scatter_offset (p0, z1, 7, z0))

/*
** st1w_scatter_8_s64_offset:
**	st1w	z0\.d, p0, \[z1\.d, #8\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_8_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 8, z0),
		       svst1w_scatter_offset (p0, z1, 8, z0))

/*
** st1w_scatter_124_s64_offset:
**	st1w	z0\.d, p0, \[z1\.d, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_124_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 124, z0),
		       svst1w_scatter_offset (p0, z1, 124, z0))

/*
** st1w_scatter_128_s64_offset:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_128_s64_offset, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_offset_s64 (p0, z1, 128, z0),
		       svst1w_scatter_offset (p0, z1, 128, z0))

/*
** st1w_scatter_x0_s64_index:
**	lsl	(x[0-9]+), x0, #?2
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_x0_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, x0, z0),
		       svst1w_scatter_index (p0, z1, x0, z0))

/*
** st1w_scatter_m1_s64_index:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_m1_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, -1, z0),
		       svst1w_scatter_index (p0, z1, -1, z0))

/*
** st1w_scatter_0_s64_index:
**	st1w	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_0_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, 0, z0),
		       svst1w_scatter_index (p0, z1, 0, z0))

/*
** st1w_scatter_5_s64_index:
**	st1w	z0\.d, p0, \[z1\.d, #20\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_5_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, 5, z0),
		       svst1w_scatter_index (p0, z1, 5, z0))

/*
** st1w_scatter_31_s64_index:
**	st1w	z0\.d, p0, \[z1\.d, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_31_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, 31, z0),
		       svst1w_scatter_index (p0, z1, 31, z0))

/*
** st1w_scatter_32_s64_index:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1w_scatter_32_s64_index, svint64_t, svuint64_t,
		       svst1w_scatter_u64base_index_s64 (p0, z1, 32, z0),
		       svst1w_scatter_index (p0, z1, 32, z0))

/*
** st1w_scatter_x0_s64_s64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_x0_s64_s64offset, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svst1w_scatter_offset (p0, x0, z1, z0))

/*
** st1w_scatter_s64_s64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_s64_s64offset, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svst1w_scatter_offset (p0, x0, z1, z0))

/*
** st1w_scatter_ext_s64_s64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_ext_s64_s64offset, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64offset_s64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1w_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1w_scatter_x0_s64_u64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_x0_s64_u64offset, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svst1w_scatter_offset (p0, x0, z1, z0))

/*
** st1w_scatter_s64_u64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_s64_u64offset, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svst1w_scatter_offset (p0, x0, z1, z0))

/*
** st1w_scatter_ext_s64_u64offset:
**	st1w	z0\.d, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_ext_s64_u64offset, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64offset_s64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1w_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1w_scatter_x0_s64_s64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_x0_s64_s64index, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64index_s64 (p0, x0, z1, z0),
		       svst1w_scatter_index (p0, x0, z1, z0))

/*
** st1w_scatter_s64_s64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_s64_s64index, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64index_s64 (p0, x0, z1, z0),
		       svst1w_scatter_index (p0, x0, z1, z0))

/*
** st1w_scatter_ext_s64_s64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, sxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_ext_s64_s64index, svint64_t, int32_t, svint64_t,
		       svst1w_scatter_s64index_s64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1w_scatter_index (p0, x0, svextw_x (p0, z1), z0))

/*
** st1w_scatter_x0_s64_u64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_x0_s64_u64index, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64index_s64 (p0, x0, z1, z0),
		       svst1w_scatter_index (p0, x0, z1, z0))

/*
** st1w_scatter_s64_u64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_s64_u64index, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64index_s64 (p0, x0, z1, z0),
		       svst1w_scatter_index (p0, x0, z1, z0))

/*
** st1w_scatter_ext_s64_u64index:
**	st1w	z0\.d, p0, \[x0, z1\.d, uxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1w_scatter_ext_s64_u64index, svint64_t, int32_t, svuint64_t,
		       svst1w_scatter_u64index_s64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1w_scatter_index (p0, x0, svextw_x (p0, z1), z0))
