/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfw_gather_u32base:
**	prfw	pldl1keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_u32base, svuint32_t,
			 svprfw_gather_u32base (p0, z0, SV_PLDL1KEEP),
			 svprfw_gather (p0, z0, SV_PLDL1KEEP))

/*
** prfw_gather_u64base:
**	prfw	pldl1strm, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_u64base, svuint64_t,
			 svprfw_gather_u64base (p0, z0, SV_PLDL1STRM),
			 svprfw_gather (p0, z0, SV_PLDL1STRM))

/*
** prfw_gather_x0_u32base_index:
**	lsl	(x[0-9]+), x0, #?2
**	prfb	pldl2keep, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_x0_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, x0, SV_PLDL2KEEP),
			 svprfw_gather_index (p0, z0, x0, SV_PLDL2KEEP))

/*
** prfw_gather_m1_u32base_index:
**	mov	(x[0-9]+), #?-4
**	prfb	pldl2strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_m1_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, -1, SV_PLDL2STRM),
			 svprfw_gather_index (p0, z0, -1, SV_PLDL2STRM))

/*
** prfw_gather_0_u32base_index:
**	prfw	pldl3keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_0_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, 0, SV_PLDL3KEEP),
			 svprfw_gather_index (p0, z0, 0, SV_PLDL3KEEP))

/*
** prfw_gather_5_u32base_index:
**	prfw	pldl3strm, p0, \[z0\.s, #20\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_5_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, 5, SV_PLDL3STRM),
			 svprfw_gather_index (p0, z0, 5, SV_PLDL3STRM))

/*
** prfw_gather_31_u32base_index:
**	prfw	pstl1keep, p0, \[z0\.s, #124\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_31_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, 31, SV_PSTL1KEEP),
			 svprfw_gather_index (p0, z0, 31, SV_PSTL1KEEP))

/*
** prfw_gather_32_u32base_index:
**	mov	(x[0-9]+), #?128
**	prfb	pstl1strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_32_u32base_index, svuint32_t,
			 svprfw_gather_u32base_index (p0, z0, 32, SV_PSTL1STRM),
			 svprfw_gather_index (p0, z0, 32, SV_PSTL1STRM))

/*
** prfw_gather_x0_u64base_index:
**	lsl	(x[0-9]+), x0, #?2
**	prfb	pstl2keep, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_x0_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, x0, SV_PSTL2KEEP),
			 svprfw_gather_index (p0, z0, x0, SV_PSTL2KEEP))

/*
** prfw_gather_m1_u64base_index:
**	mov	(x[0-9]+), #?-4
**	prfb	pstl2strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_m1_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, -1, SV_PSTL2STRM),
			 svprfw_gather_index (p0, z0, -1, SV_PSTL2STRM))

/*
** prfw_gather_0_u64base_index:
**	prfw	pstl3keep, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_0_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, 0, SV_PSTL3KEEP),
			 svprfw_gather_index (p0, z0, 0, SV_PSTL3KEEP))

/*
** prfw_gather_5_u64base_index:
**	prfw	pstl3strm, p0, \[z0\.d, #20\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_5_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, 5, SV_PSTL3STRM),
			 svprfw_gather_index (p0, z0, 5, SV_PSTL3STRM))

/*
** prfw_gather_31_u64base_index:
**	prfw	pldl1keep, p0, \[z0\.d, #124\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_31_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, 31, SV_PLDL1KEEP),
			 svprfw_gather_index (p0, z0, 31, SV_PLDL1KEEP))

/*
** prfw_gather_32_u64base_index:
**	mov	(x[0-9]+), #?128
**	prfb	pldl1strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfw_gather_32_u64base_index, svuint64_t,
			 svprfw_gather_u64base_index (p0, z0, 32, SV_PLDL1STRM),
			 svprfw_gather_index (p0, z0, 32, SV_PLDL1STRM))

/*
** prfw_gather_x0_s32index:
**	prfw	pldl2keep, p0, \[x0, z0\.s, sxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_x0_s32index, svint32_t,
			 svprfw_gather_s32index (p0, x0, z0, SV_PLDL2KEEP),
			 svprfw_gather_index (p0, x0, z0, SV_PLDL2KEEP))

/*
** prfw_gather_s32index:
**	prfw	pldl2strm, p0, \[x0, z1\.s, sxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_s32index, svint32_t,
			 svprfw_gather_s32index (p0, x0, z1, SV_PLDL2STRM),
			 svprfw_gather_index (p0, x0, z1, SV_PLDL2STRM))

/*
** prfw_gather_x0_u32index:
**	prfw	pldl3keep, p0, \[x0, z0\.s, uxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_x0_u32index, svuint32_t,
			 svprfw_gather_u32index (p0, x0, z0, SV_PLDL3KEEP),
			 svprfw_gather_index (p0, x0, z0, SV_PLDL3KEEP))

/*
** prfw_gather_u32index:
**	prfw	pldl3strm, p0, \[x0, z1\.s, uxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_u32index, svuint32_t,
			 svprfw_gather_u32index (p0, x0, z1, SV_PLDL3STRM),
			 svprfw_gather_index (p0, x0, z1, SV_PLDL3STRM))

/*
** prfw_gather_x0_s64index:
**	prfw	pstl1keep, p0, \[x0, z0\.d, lsl 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_x0_s64index, svint64_t,
			 svprfw_gather_s64index (p0, x0, z0, SV_PSTL1KEEP),
			 svprfw_gather_index (p0, x0, z0, SV_PSTL1KEEP))

/*
** prfw_gather_s64index:
**	prfw	pstl1strm, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_s64index, svint64_t,
			 svprfw_gather_s64index (p0, x0, z1, SV_PSTL1STRM),
			 svprfw_gather_index (p0, x0, z1, SV_PSTL1STRM))

/*
** prfw_gather_ext_s64index:
**	prfw	pstl1strm, p0, \[x0, z1\.d, sxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_ext_s64index, svint64_t,
			 svprfw_gather_s64index (p0, x0, svextw_s64_x (p0, z1), SV_PSTL1STRM),
			 svprfw_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL1STRM))

/*
** prfw_gather_x0_u64index:
**	prfw	pstl2keep, p0, \[x0, z0\.d, lsl 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_x0_u64index, svuint64_t,
			 svprfw_gather_u64index (p0, x0, z0, SV_PSTL2KEEP),
			 svprfw_gather_index (p0, x0, z0, SV_PSTL2KEEP))

/*
** prfw_gather_u64index:
**	prfw	pstl2strm, p0, \[x0, z1\.d, lsl 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_u64index, svuint64_t,
			 svprfw_gather_u64index (p0, x0, z1, SV_PSTL2STRM),
			 svprfw_gather_index (p0, x0, z1, SV_PSTL2STRM))

/*
** prfw_gather_ext_u64index:
**	prfw	pstl2strm, p0, \[x0, z1\.d, uxtw 2\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfw_gather_ext_u64index, svuint64_t,
			 svprfw_gather_u64index (p0, x0, svextw_u64_x (p0, z1), SV_PSTL2STRM),
			 svprfw_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL2STRM))
