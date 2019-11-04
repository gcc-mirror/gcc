/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** st1_scatter_s32:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_s32, svint32_t, svuint32_t,
		       svst1_scatter_u32base_s32 (p0, z1, z0),
		       svst1_scatter (p0, z1, z0))

/*
** st1_scatter_x0_s32_offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, x0, z0),
		       svst1_scatter_offset (p0, z1, x0, z0))

/*
** st1_scatter_m4_s32_offset:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m4_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, -4, z0),
		       svst1_scatter_offset (p0, z1, -4, z0))

/*
** st1_scatter_0_s32_offset:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 0, z0),
		       svst1_scatter_offset (p0, z1, 0, z0))

/*
** st1_scatter_5_s32_offset:
**	mov	(x[0-9]+), #?5
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_5_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 5, z0),
		       svst1_scatter_offset (p0, z1, 5, z0))

/*
** st1_scatter_6_s32_offset:
**	mov	(x[0-9]+), #?6
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_6_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 6, z0),
		       svst1_scatter_offset (p0, z1, 6, z0))

/*
** st1_scatter_7_s32_offset:
**	mov	(x[0-9]+), #?7
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_7_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 7, z0),
		       svst1_scatter_offset (p0, z1, 7, z0))

/*
** st1_scatter_8_s32_offset:
**	st1w	z0\.s, p0, \[z1\.s, #8\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_8_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 8, z0),
		       svst1_scatter_offset (p0, z1, 8, z0))

/*
** st1_scatter_124_s32_offset:
**	st1w	z0\.s, p0, \[z1\.s, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_124_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 124, z0),
		       svst1_scatter_offset (p0, z1, 124, z0))

/*
** st1_scatter_128_s32_offset:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_128_s32_offset, svint32_t, svuint32_t,
		       svst1_scatter_u32base_offset_s32 (p0, z1, 128, z0),
		       svst1_scatter_offset (p0, z1, 128, z0))

/*
** st1_scatter_x0_s32_index:
**	lsl	(x[0-9]+), x0, #?2
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_x0_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, x0, z0),
		       svst1_scatter_index (p0, z1, x0, z0))

/*
** st1_scatter_m1_s32_index:
**	mov	(x[0-9]+), #?-4
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_m1_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, -1, z0),
		       svst1_scatter_index (p0, z1, -1, z0))

/*
** st1_scatter_0_s32_index:
**	st1w	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_0_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, 0, z0),
		       svst1_scatter_index (p0, z1, 0, z0))

/*
** st1_scatter_5_s32_index:
**	st1w	z0\.s, p0, \[z1\.s, #20\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_5_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, 5, z0),
		       svst1_scatter_index (p0, z1, 5, z0))

/*
** st1_scatter_31_s32_index:
**	st1w	z0\.s, p0, \[z1\.s, #124\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_31_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, 31, z0),
		       svst1_scatter_index (p0, z1, 31, z0))

/*
** st1_scatter_32_s32_index:
**	mov	(x[0-9]+), #?128
**	st1w	z0\.s, p0, \[\1, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_ZS (st1_scatter_32_s32_index, svint32_t, svuint32_t,
		       svst1_scatter_u32base_index_s32 (p0, z1, 32, z0),
		       svst1_scatter_index (p0, z1, 32, z0))

/*
** st1_scatter_x0_s32_s32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_s32_s32offset, svint32_t, int32_t, svint32_t,
		       svst1_scatter_s32offset_s32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_s32_s32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_s32_s32offset, svint32_t, int32_t, svint32_t,
		       svst1_scatter_s32offset_s32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_x0_s32_u32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_s32_u32offset, svint32_t, int32_t, svuint32_t,
		       svst1_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_s32_u32offset:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_s32_u32offset, svint32_t, int32_t, svuint32_t,
		       svst1_scatter_u32offset_s32 (p0, x0, z1, z0),
		       svst1_scatter_offset (p0, x0, z1, z0))

/*
** st1_scatter_x0_s32_s32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_s32_s32index, svint32_t, int32_t, svint32_t,
		       svst1_scatter_s32index_s32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_s32_s32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_s32_s32index, svint32_t, int32_t, svint32_t,
		       svst1_scatter_s32index_s32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_x0_s32_u32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_x0_s32_u32index, svint32_t, int32_t, svuint32_t,
		       svst1_scatter_u32index_s32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))

/*
** st1_scatter_s32_u32index:
**	st1w	z0\.s, p0, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_STORE_SCATTER_SZ (st1_scatter_s32_u32index, svint32_t, int32_t, svuint32_t,
		       svst1_scatter_u32index_s32 (p0, x0, z1, z0),
		       svst1_scatter_index (p0, x0, z1, z0))
