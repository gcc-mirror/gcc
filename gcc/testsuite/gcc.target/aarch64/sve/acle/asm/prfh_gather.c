/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** prfh_gather_u32base:
**	prfh	pldl1keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_u32base, svuint32_t,
			 svprfh_gather_u32base (p0, z0, SV_PLDL1KEEP),
			 svprfh_gather (p0, z0, SV_PLDL1KEEP))

/*
** prfh_gather_u64base:
**	prfh	pldl1strm, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_u64base, svuint64_t,
			 svprfh_gather_u64base (p0, z0, SV_PLDL1STRM),
			 svprfh_gather (p0, z0, SV_PLDL1STRM))

/*
** prfh_gather_x0_u32base_index:
**	lsl	(x[0-9]+), x0, #?1
**	prfb	pldl2keep, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_x0_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, x0, SV_PLDL2KEEP),
			 svprfh_gather_index (p0, z0, x0, SV_PLDL2KEEP))

/*
** prfh_gather_m1_u32base_index:
**	mov	(x[0-9]+), #?-2
**	prfb	pldl2strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_m1_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, -1, SV_PLDL2STRM),
			 svprfh_gather_index (p0, z0, -1, SV_PLDL2STRM))

/*
** prfh_gather_0_u32base_index:
**	prfh	pldl3keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_0_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, 0, SV_PLDL3KEEP),
			 svprfh_gather_index (p0, z0, 0, SV_PLDL3KEEP))

/*
** prfh_gather_5_u32base_index:
**	prfh	pldl3strm, p0, \[z0\.s, #10\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_5_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, 5, SV_PLDL3STRM),
			 svprfh_gather_index (p0, z0, 5, SV_PLDL3STRM))

/*
** prfh_gather_31_u32base_index:
**	prfh	pstl1keep, p0, \[z0\.s, #62\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_31_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, 31, SV_PSTL1KEEP),
			 svprfh_gather_index (p0, z0, 31, SV_PSTL1KEEP))

/*
** prfh_gather_32_u32base_index:
**	mov	(x[0-9]+), #?64
**	prfb	pstl1strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_32_u32base_index, svuint32_t,
			 svprfh_gather_u32base_index (p0, z0, 32, SV_PSTL1STRM),
			 svprfh_gather_index (p0, z0, 32, SV_PSTL1STRM))

/*
** prfh_gather_x0_u64base_index:
**	lsl	(x[0-9]+), x0, #?1
**	prfb	pstl2keep, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_x0_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, x0, SV_PSTL2KEEP),
			 svprfh_gather_index (p0, z0, x0, SV_PSTL2KEEP))

/*
** prfh_gather_m1_u64base_index:
**	mov	(x[0-9]+), #?-2
**	prfb	pstl2strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_m1_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, -1, SV_PSTL2STRM),
			 svprfh_gather_index (p0, z0, -1, SV_PSTL2STRM))

/*
** prfh_gather_0_u64base_index:
**	prfh	pstl3keep, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_0_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, 0, SV_PSTL3KEEP),
			 svprfh_gather_index (p0, z0, 0, SV_PSTL3KEEP))

/*
** prfh_gather_5_u64base_index:
**	prfh	pstl3strm, p0, \[z0\.d, #10\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_5_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, 5, SV_PSTL3STRM),
			 svprfh_gather_index (p0, z0, 5, SV_PSTL3STRM))

/*
** prfh_gather_31_u64base_index:
**	prfh	pldl1keep, p0, \[z0\.d, #62\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_31_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, 31, SV_PLDL1KEEP),
			 svprfh_gather_index (p0, z0, 31, SV_PLDL1KEEP))

/*
** prfh_gather_32_u64base_index:
**	mov	(x[0-9]+), #?64
**	prfb	pldl1strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfh_gather_32_u64base_index, svuint64_t,
			 svprfh_gather_u64base_index (p0, z0, 32, SV_PLDL1STRM),
			 svprfh_gather_index (p0, z0, 32, SV_PLDL1STRM))

/*
** prfh_gather_x0_s32index:
**	prfh	pldl2keep, p0, \[x0, z0\.s, sxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_x0_s32index, svint32_t,
			 svprfh_gather_s32index (p0, x0, z0, SV_PLDL2KEEP),
			 svprfh_gather_index (p0, x0, z0, SV_PLDL2KEEP))

/*
** prfh_gather_s32index:
**	prfh	pldl2strm, p0, \[x0, z1\.s, sxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_s32index, svint32_t,
			 svprfh_gather_s32index (p0, x0, z1, SV_PLDL2STRM),
			 svprfh_gather_index (p0, x0, z1, SV_PLDL2STRM))

/*
** prfh_gather_x0_u32index:
**	prfh	pldl3keep, p0, \[x0, z0\.s, uxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_x0_u32index, svuint32_t,
			 svprfh_gather_u32index (p0, x0, z0, SV_PLDL3KEEP),
			 svprfh_gather_index (p0, x0, z0, SV_PLDL3KEEP))

/*
** prfh_gather_u32index:
**	prfh	pldl3strm, p0, \[x0, z1\.s, uxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_u32index, svuint32_t,
			 svprfh_gather_u32index (p0, x0, z1, SV_PLDL3STRM),
			 svprfh_gather_index (p0, x0, z1, SV_PLDL3STRM))

/*
** prfh_gather_x0_s64index:
**	prfh	pstl1keep, p0, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_x0_s64index, svint64_t,
			 svprfh_gather_s64index (p0, x0, z0, SV_PSTL1KEEP),
			 svprfh_gather_index (p0, x0, z0, SV_PSTL1KEEP))

/*
** prfh_gather_s64index:
**	prfh	pstl1strm, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_s64index, svint64_t,
			 svprfh_gather_s64index (p0, x0, z1, SV_PSTL1STRM),
			 svprfh_gather_index (p0, x0, z1, SV_PSTL1STRM))

/*
** prfh_gather_ext_s64index:
**	prfh	pstl1strm, p0, \[x0, z1\.d, sxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_ext_s64index, svint64_t,
			 svprfh_gather_s64index (p0, x0, svextw_s64_x (p0, z1), SV_PSTL1STRM),
			 svprfh_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL1STRM))

/*
** prfh_gather_x0_u64index:
**	prfh	pstl2keep, p0, \[x0, z0\.d, lsl 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_x0_u64index, svuint64_t,
			 svprfh_gather_u64index (p0, x0, z0, SV_PSTL2KEEP),
			 svprfh_gather_index (p0, x0, z0, SV_PSTL2KEEP))

/*
** prfh_gather_u64index:
**	prfh	pstl2strm, p0, \[x0, z1\.d, lsl 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_u64index, svuint64_t,
			 svprfh_gather_u64index (p0, x0, z1, SV_PSTL2STRM),
			 svprfh_gather_index (p0, x0, z1, SV_PSTL2STRM))

/*
** prfh_gather_ext_u64index:
**	prfh	pstl2strm, p0, \[x0, z1\.d, uxtw 1\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfh_gather_ext_u64index, svuint64_t,
			 svprfh_gather_u64index (p0, x0, svextw_u64_x (p0, z1), SV_PSTL2STRM),
			 svprfh_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL2STRM))
