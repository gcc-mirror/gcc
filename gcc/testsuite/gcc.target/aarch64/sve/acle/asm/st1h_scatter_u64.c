/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st1h_scatter_u64:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_u64, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_u64 (p0, z1, z0),
		       svst1h_scatter (p0, z1, z0))

/*
** st1h_scatter_x0_u64_offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, x0, z0),
		       svst1h_scatter_offset (p0, z1, x0, z0))

/*
** st1h_scatter_m2_u64_offset:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m2_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, -2, z0),
		       svst1h_scatter_offset (p0, z1, -2, z0))

/*
** st1h_scatter_0_u64_offset:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, 0, z0),
		       svst1h_scatter_offset (p0, z1, 0, z0))

/*
** st1h_scatter_5_u64_offset:
**	mov	(x[0-9]+), #?5
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, 5, z0),
		       svst1h_scatter_offset (p0, z1, 5, z0))

/*
** st1h_scatter_6_u64_offset:
**	st1h	z0\.d, p0, \[z1\.d, #6\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_6_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, 6, z0),
		       svst1h_scatter_offset (p0, z1, 6, z0))

/*
** st1h_scatter_62_u64_offset:
**	st1h	z0\.d, p0, \[z1\.d, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_62_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, 62, z0),
		       svst1h_scatter_offset (p0, z1, 62, z0))

/*
** st1h_scatter_64_u64_offset:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_64_u64_offset, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_offset_u64 (p0, z1, 64, z0),
		       svst1h_scatter_offset (p0, z1, 64, z0))

/*
** st1h_scatter_x0_u64_index:
**	lsl	(x[0-9]+), x0, #?1
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_x0_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, x0, z0),
		       svst1h_scatter_index (p0, z1, x0, z0))

/*
** st1h_scatter_m1_u64_index:
**	mov	(x[0-9]+), #?-2
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_m1_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, -1, z0),
		       svst1h_scatter_index (p0, z1, -1, z0))

/*
** st1h_scatter_0_u64_index:
**	st1h	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_0_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, 0, z0),
		       svst1h_scatter_index (p0, z1, 0, z0))

/*
** st1h_scatter_5_u64_index:
**	st1h	z0\.d, p0, \[z1\.d, #10\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_5_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, 5, z0),
		       svst1h_scatter_index (p0, z1, 5, z0))

/*
** st1h_scatter_31_u64_index:
**	st1h	z0\.d, p0, \[z1\.d, #62\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_31_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, 31, z0),
		       svst1h_scatter_index (p0, z1, 31, z0))

/*
** st1h_scatter_32_u64_index:
**	mov	(x[0-9]+), #?64
**	st1h	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1h_scatter_32_u64_index, svuint64_t, svuint64_t,
		       svst1h_scatter_u64base_index_u64 (p0, z1, 32, z0),
		       svst1h_scatter_index (p0, z1, 32, z0))

/*
** st1h_scatter_x0_u64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u64_s64offset, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_u64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u64_s64offset, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_ext_u64_s64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_u64_s64offset, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64offset_u64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1h_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_u64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u64_u64offset, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_u64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u64_u64offset, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1h_scatter_offset (p0, x0, z1, z0))

/*
** st1h_scatter_ext_u64_u64offset:
**	st1h	z0\.d, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_u64_u64offset, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64offset_u64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1h_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_u64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u64_s64index, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64index_u64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_u64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u64_s64index, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64index_u64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_ext_u64_s64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, sxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_u64_s64index, svuint64_t, uint16_t, svint64_t,
		       svst1h_scatter_s64index_u64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1h_scatter_index (p0, x0, svextw_x (p0, z1), z0))

/*
** st1h_scatter_x0_u64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_x0_u64_u64index, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64index_u64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_u64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_u64_u64index, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64index_u64 (p0, x0, z1, z0),
		       svst1h_scatter_index (p0, x0, z1, z0))

/*
** st1h_scatter_ext_u64_u64index:
**	st1h	z0\.d, p0, \[x0, z1\.d, uxtw 1\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1h_scatter_ext_u64_u64index, svuint64_t, uint16_t, svuint64_t,
		       svst1h_scatter_u64index_u64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1h_scatter_index (p0, x0, svextw_x (p0, z1), z0))
