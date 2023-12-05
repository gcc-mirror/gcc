/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** st1b_scatter_u64:
**	st1b	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_u64, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_u64 (p0, z1, z0),
		       svst1b_scatter (p0, z1, z0))

/*
** st1b_scatter_x0_u64_offset:
**	st1b	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_x0_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, x0, z0),
		       svst1b_scatter_offset (p0, z1, x0, z0))

/*
** st1b_scatter_m1_u64_offset:
**	mov	(x[0-9]+), #?-1
**	st1b	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_m1_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, -1, z0),
		       svst1b_scatter_offset (p0, z1, -1, z0))

/*
** st1b_scatter_0_u64_offset:
**	st1b	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_0_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, 0, z0),
		       svst1b_scatter_offset (p0, z1, 0, z0))

/*
** st1b_scatter_5_u64_offset:
**	st1b	z0\.d, p0, \[z1\.d, #5\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_5_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, 5, z0),
		       svst1b_scatter_offset (p0, z1, 5, z0))

/*
** st1b_scatter_31_u64_offset:
**	st1b	z0\.d, p0, \[z1\.d, #31\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_31_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, 31, z0),
		       svst1b_scatter_offset (p0, z1, 31, z0))

/*
** st1b_scatter_32_u64_offset:
**	mov	(x[0-9]+), #?32
**	st1b	z0\.d, p0, \[\1, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1b_scatter_32_u64_offset, svuint64_t, svuint64_t,
		       svst1b_scatter_u64base_offset_u64 (p0, z1, 32, z0),
		       svst1b_scatter_offset (p0, z1, 32, z0))

/*
** st1b_scatter_x0_u64_s64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_x0_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		       svst1b_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1b_scatter_offset (p0, x0, z1, z0))

/*
** st1b_scatter_u64_s64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		       svst1b_scatter_s64offset_u64 (p0, x0, z1, z0),
		       svst1b_scatter_offset (p0, x0, z1, z0))

/*
** st1b_scatter_ext_u64_s64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_ext_u64_s64offset, svuint64_t, uint8_t, svint64_t,
		       svst1b_scatter_s64offset_u64 (p0, x0, svextw_s64_x (p0, z1), z0),
		       svst1b_scatter_offset (p0, x0, svextw_x (p0, z1), z0))

/*
** st1b_scatter_x0_u64_u64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_x0_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		       svst1b_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1b_scatter_offset (p0, x0, z1, z0))

/*
** st1b_scatter_u64_u64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		       svst1b_scatter_u64offset_u64 (p0, x0, z1, z0),
		       svst1b_scatter_offset (p0, x0, z1, z0))

/*
** st1b_scatter_ext_u64_u64offset:
**	st1b	z0\.d, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1b_scatter_ext_u64_u64offset, svuint64_t, uint8_t, svuint64_t,
		       svst1b_scatter_u64offset_u64 (p0, x0, svextw_u64_x (p0, z1), z0),
		       svst1b_scatter_offset (p0, x0, svextw_x (p0, z1), z0))
