/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1b_scatter_s64:
**	stnt1b	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_s64, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_s64 (p0, z1, z0),
		       svstnt1b_scatter (p0, z1, z0))

/*
** stnt1b_scatter_x0_s64_offset:
**	stnt1b	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_x0_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, x0, z0),
		       svstnt1b_scatter_offset (p0, z1, x0, z0))

/*
** stnt1b_scatter_m1_s64_offset:
**	mov	(x[0-9]+), #?-1
**	stnt1b	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_m1_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, -1, z0),
		       svstnt1b_scatter_offset (p0, z1, -1, z0))

/*
** stnt1b_scatter_0_s64_offset:
**	stnt1b	z0\.d, p0, \[z1\.d\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_0_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, 0, z0),
		       svstnt1b_scatter_offset (p0, z1, 0, z0))

/*
** stnt1b_scatter_5_s64_offset:
**	mov	(x[0-9]+), #?5
**	stnt1b	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_5_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, 5, z0),
		       svstnt1b_scatter_offset (p0, z1, 5, z0))

/*
** stnt1b_scatter_31_s64_offset:
**	mov	(x[0-9]+), #?31
**	stnt1b	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_31_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, 31, z0),
		       svstnt1b_scatter_offset (p0, z1, 31, z0))

/*
** stnt1b_scatter_32_s64_offset:
**	mov	(x[0-9]+), #?32
**	stnt1b	z0\.d, p0, \[z1\.d, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_32_s64_offset, svint64_t, svuint64_t,
		       svstnt1b_scatter_u64base_offset_s64 (p0, z1, 32, z0),
		       svstnt1b_scatter_offset (p0, z1, 32, z0))

/*
** stnt1b_scatter_x0_s64_s64offset:
**	stnt1b	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_x0_s64_s64offset, svint64_t, int8_t, svint64_t,
		       svstnt1b_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))

/*
** stnt1b_scatter_s64_s64offset:
**	stnt1b	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_s64_s64offset, svint64_t, int8_t, svint64_t,
		       svstnt1b_scatter_s64offset_s64 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))

/*
** stnt1b_scatter_x0_s64_u64offset:
**	stnt1b	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_x0_s64_u64offset, svint64_t, int8_t, svuint64_t,
		       svstnt1b_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))

/*
** stnt1b_scatter_s64_u64offset:
**	stnt1b	z0\.d, p0, \[z1\.d, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_s64_u64offset, svint64_t, int8_t, svuint64_t,
		       svstnt1b_scatter_u64offset_s64 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))
