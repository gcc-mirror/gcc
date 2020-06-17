/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1_scatter_s32:
**	stnt1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_s32, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_s32 (p0, z1, z0),
		       svstnt1_scatter (p0, z1, z0))

/*
** stnt1_scatter_x0_s32_offset:
**	stnt1w	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_x0_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, x0, z0),
		       svstnt1_scatter_offset (p0, z1, x0, z0))

/*
** stnt1_scatter_m4_s32_offset:
**	mov	(x[0-9]+), #?-4
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_m4_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, -4, z0),
		       svstnt1_scatter_offset (p0, z1, -4, z0))

/*
** stnt1_scatter_0_s32_offset:
**	stnt1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_0_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 0, z0),
		       svstnt1_scatter_offset (p0, z1, 0, z0))

/*
** stnt1_scatter_5_s32_offset:
**	mov	(x[0-9]+), #?5
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_5_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 5, z0),
		       svstnt1_scatter_offset (p0, z1, 5, z0))

/*
** stnt1_scatter_6_s32_offset:
**	mov	(x[0-9]+), #?6
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_6_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 6, z0),
		       svstnt1_scatter_offset (p0, z1, 6, z0))

/*
** stnt1_scatter_7_s32_offset:
**	mov	(x[0-9]+), #?7
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_7_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 7, z0),
		       svstnt1_scatter_offset (p0, z1, 7, z0))

/*
** stnt1_scatter_8_s32_offset:
**	mov	(x[0-9]+), #?8
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_8_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 8, z0),
		       svstnt1_scatter_offset (p0, z1, 8, z0))

/*
** stnt1_scatter_124_s32_offset:
**	mov	(x[0-9]+), #?124
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_124_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 124, z0),
		       svstnt1_scatter_offset (p0, z1, 124, z0))

/*
** stnt1_scatter_128_s32_offset:
**	mov	(x[0-9]+), #?128
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_128_s32_offset, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_offset_s32 (p0, z1, 128, z0),
		       svstnt1_scatter_offset (p0, z1, 128, z0))

/*
** stnt1_scatter_x0_s32_index:
**	lsl	(x[0-9]+), x0, #?2
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_x0_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, x0, z0),
		       svstnt1_scatter_index (p0, z1, x0, z0))

/*
** stnt1_scatter_m1_s32_index:
**	mov	(x[0-9]+), #?-4
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_m1_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, -1, z0),
		       svstnt1_scatter_index (p0, z1, -1, z0))

/*
** stnt1_scatter_0_s32_index:
**	stnt1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_0_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, 0, z0),
		       svstnt1_scatter_index (p0, z1, 0, z0))

/*
** stnt1_scatter_5_s32_index:
**	mov	(x[0-9]+), #?20
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_5_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, 5, z0),
		       svstnt1_scatter_index (p0, z1, 5, z0))

/*
** stnt1_scatter_31_s32_index:
**	mov	(x[0-9]+), #?124
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_31_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, 31, z0),
		       svstnt1_scatter_index (p0, z1, 31, z0))

/*
** stnt1_scatter_32_s32_index:
**	mov	(x[0-9]+), #?128
**	stnt1w	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1_scatter_32_s32_index, svint32_t, svuint32_t,
		       svstnt1_scatter_u32base_index_s32 (p0, z1, 32, z0),
		       svstnt1_scatter_index (p0, z1, 32, z0))

/*
** stnt1_scatter_x0_s32_u32offset:
**	stnt1w	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_x0_s32_u32offset, svint32_t, int32_t, svuint32_t,
		       svstnt1_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))

/*
** stnt1_scatter_s32_u32offset:
**	stnt1w	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1_scatter_s32_u32offset, svint32_t, int32_t, svuint32_t,
		       svstnt1_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svstnt1_scatter_offset (p0, x0, z1, z0))
