/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** stnt1b_scatter_u32:
**	stnt1b	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_u32, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_u32 (p0, z1, z0),
		       svstnt1b_scatter (p0, z1, z0))

/*
** stnt1b_scatter_x0_u32_offset:
**	stnt1b	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_x0_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, x0, z0),
		       svstnt1b_scatter_offset (p0, z1, x0, z0))

/*
** stnt1b_scatter_m1_u32_offset:
**	mov	(x[0-9]+), #?-1
**	stnt1b	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_m1_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, -1, z0),
		       svstnt1b_scatter_offset (p0, z1, -1, z0))

/*
** stnt1b_scatter_0_u32_offset:
**	stnt1b	z0\.s, p0, \[z1\.s\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_0_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, 0, z0),
		       svstnt1b_scatter_offset (p0, z1, 0, z0))

/*
** stnt1b_scatter_5_u32_offset:
**	mov	(x[0-9]+), #?5
**	stnt1b	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_5_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, 5, z0),
		       svstnt1b_scatter_offset (p0, z1, 5, z0))

/*
** stnt1b_scatter_31_u32_offset:
**	mov	(x[0-9]+), #?31
**	stnt1b	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_31_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, 31, z0),
		       svstnt1b_scatter_offset (p0, z1, 31, z0))

/*
** stnt1b_scatter_32_u32_offset:
**	mov	(x[0-9]+), #?32
**	stnt1b	z0\.s, p0, \[z1\.s, \1\]
**	ret
*/
TEST_STORE_SCATTER_ZS (stnt1b_scatter_32_u32_offset, svuint32_t, svuint32_t,
		       svstnt1b_scatter_u32base_offset_u32 (p0, z1, 32, z0),
		       svstnt1b_scatter_offset (p0, z1, 32, z0))

/*
** stnt1b_scatter_x0_u32_u32offset:
**	stnt1b	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_x0_u32_u32offset, svuint32_t, uint8_t, svuint32_t,
		       svstnt1b_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))

/*
** stnt1b_scatter_u32_u32offset:
**	stnt1b	z0\.s, p0, \[z1\.s, x0\]
**	ret
*/
TEST_STORE_SCATTER_SZ (stnt1b_scatter_u32_u32offset, svuint32_t, uint8_t, svuint32_t,
		       svstnt1b_scatter_u32offset_u32 (p0, x0, z1, z0),
		       svstnt1b_scatter_offset (p0, x0, z1, z0))
