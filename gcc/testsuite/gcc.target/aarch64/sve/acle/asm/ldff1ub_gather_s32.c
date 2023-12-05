/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1ub_gather_s32_tied1:
**	ldff1b	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_s32_tied1, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_s32 (p0, z0),
		     z0_res = svldff1ub_gather_s32 (p0, z0))

/*
** ldff1ub_gather_s32_untied:
**	ldff1b	z0\.s, p0/z, \[z1\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_s32_untied, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_s32 (p0, z1),
		     z0_res = svldff1ub_gather_s32 (p0, z1))

/*
** ldff1ub_gather_x0_s32_offset:
**	ldff1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_x0_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, x0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, x0))

/*
** ldff1ub_gather_m1_s32_offset:
**	mov	(x[0-9]+), #?-1
**	ldff1b	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_m1_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, -1),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, -1))

/*
** ldff1ub_gather_0_s32_offset:
**	ldff1b	z0\.s, p0/z, \[z0\.s\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_0_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, 0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, 0))

/*
** ldff1ub_gather_5_s32_offset:
**	ldff1b	z0\.s, p0/z, \[z0\.s, #5\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_5_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, 5),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, 5))

/*
** ldff1ub_gather_31_s32_offset:
**	ldff1b	z0\.s, p0/z, \[z0\.s, #31\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_31_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, 31),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, 31))

/*
** ldff1ub_gather_32_s32_offset:
**	mov	(x[0-9]+), #?32
**	ldff1b	z0\.s, p0/z, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldff1ub_gather_32_s32_offset, svint32_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32base_offset_s32 (p0, z0, 32),
		     z0_res = svldff1ub_gather_offset_s32 (p0, z0, 32))

/*
** ldff1ub_gather_x0_s32_s32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_x0_s32_s32offset, svint32_t, uint8_t, svint32_t,
		     z0_res = svldff1ub_gather_s32offset_s32 (p0, x0, z0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z0))

/*
** ldff1ub_gather_tied1_s32_s32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_tied1_s32_s32offset, svint32_t, uint8_t, svint32_t,
		     z0_res = svldff1ub_gather_s32offset_s32 (p0, x0, z0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z0))

/*
** ldff1ub_gather_untied_s32_s32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_untied_s32_s32offset, svint32_t, uint8_t, svint32_t,
		     z0_res = svldff1ub_gather_s32offset_s32 (p0, x0, z1),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z1))

/*
** ldff1ub_gather_x0_s32_u32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_x0_s32_u32offset, svint32_t, uint8_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32offset_s32 (p0, x0, z0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z0))

/*
** ldff1ub_gather_tied1_s32_u32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_tied1_s32_u32offset, svint32_t, uint8_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32offset_s32 (p0, x0, z0),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z0))

/*
** ldff1ub_gather_untied_s32_u32offset:
**	ldff1b	z0\.s, p0/z, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldff1ub_gather_untied_s32_u32offset, svint32_t, uint8_t, svuint32_t,
		     z0_res = svldff1ub_gather_u32offset_s32 (p0, x0, z1),
		     z0_res = svldff1ub_gather_offset_s32 (p0, x0, z1))
