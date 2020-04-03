/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1h_scatter_s32:
**	stnt1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_s32, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_s32 (p0, z1, z0),
		       svstnt1h_scatter (p0, z1, z0))

/*
** stnt1h_scatter_x0_s32_offset:
**	stnt1h	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_x0_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, x0, z0),
		       svstnt1h_scatter_offset (p0, z1, x0, z0))

/*
** stnt1h_scatter_m2_s32_offset:
**	mov	(x[0-9]+), #?-2
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_m2_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, -2, z0),
		       svstnt1h_scatter_offset (p0, z1, -2, z0))

/*
** stnt1h_scatter_0_s32_offset:
**	stnt1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_0_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, 0, z0),
		       svstnt1h_scatter_offset (p0, z1, 0, z0))

/*
** stnt1h_scatter_5_s32_offset:
**	mov	(x[0-9]+), #?5
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_5_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, 5, z0),
		       svstnt1h_scatter_offset (p0, z1, 5, z0))

/*
** stnt1h_scatter_6_s32_offset:
**	mov	(x[0-9]+), #?6
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_6_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, 6, z0),
		       svstnt1h_scatter_offset (p0, z1, 6, z0))

/*
** stnt1h_scatter_62_s32_offset:
**	mov	(x[0-9]+), #?62
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_62_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, 62, z0),
		       svstnt1h_scatter_offset (p0, z1, 62, z0))

/*
** stnt1h_scatter_64_s32_offset:
**	mov	(x[0-9]+), #?64
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_64_s32_offset, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_offset_s32 (p0, z1, 64, z0),
		       svstnt1h_scatter_offset (p0, z1, 64, z0))

/*
** stnt1h_scatter_x0_s32_index:
**	lsl	(x[0-9]+), x0, #?1
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_x0_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, x0, z0),
		       svstnt1h_scatter_index (p0, z1, x0, z0))

/*
** stnt1h_scatter_m1_s32_index:
**	mov	(x[0-9]+), #?-2
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_m1_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, -1, z0),
		       svstnt1h_scatter_index (p0, z1, -1, z0))

/*
** stnt1h_scatter_0_s32_index:
**	stnt1h	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_0_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, 0, z0),
		       svstnt1h_scatter_index (p0, z1, 0, z0))

/*
** stnt1h_scatter_5_s32_index:
**	mov	(x[0-9]+), #?10
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_5_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, 5, z0),
		       svstnt1h_scatter_index (p0, z1, 5, z0))

/*
** stnt1h_scatter_31_s32_index:
**	mov	(x[0-9]+), #?62
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_31_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, 31, z0),
		       svstnt1h_scatter_index (p0, z1, 31, z0))

/*
** stnt1h_scatter_32_s32_index:
**	mov	(x[0-9]+), #?64
**	stnt1h	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1h_scatter_32_s32_index, svint32_t, svuint32_t,
		       svstnt1h_scatter_u32base_index_s32 (p0, z1, 32, z0),
		       svstnt1h_scatter_index (p0, z1, 32, z0))

/*
** stnt1h_scatter_x0_s32_u32offset:
**	stnt1h	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_x0_s32_u32offset, svint32_t, int16_t, svuint32_t,
		       svstnt1h_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))

/*
** stnt1h_scatter_s32_u32offset:
**	stnt1h	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1h_scatter_s32_u32offset, svint32_t, int16_t, svuint32_t,
		       svstnt1h_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svstnt1h_scatter_offset (p0, x0, z1, z0))
