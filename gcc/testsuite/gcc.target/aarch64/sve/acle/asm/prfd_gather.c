/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfd_gather_u32base:
**	prfd	pldl1keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_u32base, svuint32_t,
			 svprfd_gather_u32base (p0, z0, SV_PLDL1KEEP),
			 svprfd_gather (p0, z0, SV_PLDL1KEEP))

/*
** prfd_gather_u64base:
**	prfd	pldl1strm, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_u64base, svuint64_t,
			 svprfd_gather_u64base (p0, z0, SV_PLDL1STRM),
			 svprfd_gather (p0, z0, SV_PLDL1STRM))

/*
** prfd_gather_x0_u32base_index:
**	lsl	(x[0-9]+), x0, #?3
**	prfb	pldl2keep, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_x0_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, x0, SV_PLDL2KEEP),
			 svprfd_gather_index (p0, z0, x0, SV_PLDL2KEEP))

/*
** prfd_gather_m1_u32base_index:
**	mov	(x[0-9]+), #?-8
**	prfb	pldl2strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_m1_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, -1, SV_PLDL2STRM),
			 svprfd_gather_index (p0, z0, -1, SV_PLDL2STRM))

/*
** prfd_gather_0_u32base_index:
**	prfd	pldl3keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_0_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, 0, SV_PLDL3KEEP),
			 svprfd_gather_index (p0, z0, 0, SV_PLDL3KEEP))

/*
** prfd_gather_5_u32base_index:
**	prfd	pldl3strm, p0, \[z0\.s, #40\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_5_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, 5, SV_PLDL3STRM),
			 svprfd_gather_index (p0, z0, 5, SV_PLDL3STRM))

/*
** prfd_gather_31_u32base_index:
**	prfd	pstl1keep, p0, \[z0\.s, #248\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_31_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, 31, SV_PSTL1KEEP),
			 svprfd_gather_index (p0, z0, 31, SV_PSTL1KEEP))

/*
** prfd_gather_32_u32base_index:
**	mov	(x[0-9]+), #?256
**	prfb	pstl1strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_32_u32base_index, svuint32_t,
			 svprfd_gather_u32base_index (p0, z0, 32, SV_PSTL1STRM),
			 svprfd_gather_index (p0, z0, 32, SV_PSTL1STRM))

/*
** prfd_gather_x0_u64base_index:
**	lsl	(x[0-9]+), x0, #?3
**	prfb	pstl2keep, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_x0_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, x0, SV_PSTL2KEEP),
			 svprfd_gather_index (p0, z0, x0, SV_PSTL2KEEP))

/*
** prfd_gather_m1_u64base_index:
**	mov	(x[0-9]+), #?-8
**	prfb	pstl2strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_m1_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, -1, SV_PSTL2STRM),
			 svprfd_gather_index (p0, z0, -1, SV_PSTL2STRM))

/*
** prfd_gather_0_u64base_index:
**	prfd	pstl3keep, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_0_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, 0, SV_PSTL3KEEP),
			 svprfd_gather_index (p0, z0, 0, SV_PSTL3KEEP))

/*
** prfd_gather_5_u64base_index:
**	prfd	pstl3strm, p0, \[z0\.d, #40\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_5_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, 5, SV_PSTL3STRM),
			 svprfd_gather_index (p0, z0, 5, SV_PSTL3STRM))

/*
** prfd_gather_31_u64base_index:
**	prfd	pldl1keep, p0, \[z0\.d, #248\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_31_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, 31, SV_PLDL1KEEP),
			 svprfd_gather_index (p0, z0, 31, SV_PLDL1KEEP))

/*
** prfd_gather_32_u64base_index:
**	mov	(x[0-9]+), #?256
**	prfb	pldl1strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfd_gather_32_u64base_index, svuint64_t,
			 svprfd_gather_u64base_index (p0, z0, 32, SV_PLDL1STRM),
			 svprfd_gather_index (p0, z0, 32, SV_PLDL1STRM))

/*
** prfd_gather_x0_s32index:
**	prfd	pldl2keep, p0, \[x0, z0\.s, sxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_x0_s32index, svint32_t,
			 svprfd_gather_s32index (p0, x0, z0, SV_PLDL2KEEP),
			 svprfd_gather_index (p0, x0, z0, SV_PLDL2KEEP))

/*
** prfd_gather_s32index:
**	prfd	pldl2strm, p0, \[x0, z1\.s, sxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_s32index, svint32_t,
			 svprfd_gather_s32index (p0, x0, z1, SV_PLDL2STRM),
			 svprfd_gather_index (p0, x0, z1, SV_PLDL2STRM))

/*
** prfd_gather_x0_u32index:
**	prfd	pldl3keep, p0, \[x0, z0\.s, uxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_x0_u32index, svuint32_t,
			 svprfd_gather_u32index (p0, x0, z0, SV_PLDL3KEEP),
			 svprfd_gather_index (p0, x0, z0, SV_PLDL3KEEP))

/*
** prfd_gather_u32index:
**	prfd	pldl3strm, p0, \[x0, z1\.s, uxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_u32index, svuint32_t,
			 svprfd_gather_u32index (p0, x0, z1, SV_PLDL3STRM),
			 svprfd_gather_index (p0, x0, z1, SV_PLDL3STRM))

/*
** prfd_gather_x0_s64index:
**	prfd	pstl1keep, p0, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_x0_s64index, svint64_t,
			 svprfd_gather_s64index (p0, x0, z0, SV_PSTL1KEEP),
			 svprfd_gather_index (p0, x0, z0, SV_PSTL1KEEP))

/*
** prfd_gather_s64index:
**	prfd	pstl1strm, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_s64index, svint64_t,
			 svprfd_gather_s64index (p0, x0, z1, SV_PSTL1STRM),
			 svprfd_gather_index (p0, x0, z1, SV_PSTL1STRM))

/*
** prfd_gather_ext_s64index:
**	prfd	pstl1strm, p0, \[x0, z1\.d, sxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_ext_s64index, svint64_t,
			 svprfd_gather_s64index (p0, x0, svextw_s64_x (p0, z1), SV_PSTL1STRM),
			 svprfd_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL1STRM))

/*
** prfd_gather_x0_u64index:
**	prfd	pstl2keep, p0, \[x0, z0\.d, lsl 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_x0_u64index, svuint64_t,
			 svprfd_gather_u64index (p0, x0, z0, SV_PSTL2KEEP),
			 svprfd_gather_index (p0, x0, z0, SV_PSTL2KEEP))

/*
** prfd_gather_u64index:
**	prfd	pstl2strm, p0, \[x0, z1\.d, lsl 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_u64index, svuint64_t,
			 svprfd_gather_u64index (p0, x0, z1, SV_PSTL2STRM),
			 svprfd_gather_index (p0, x0, z1, SV_PSTL2STRM))

/*
** prfd_gather_ext_u64index:
**	prfd	pstl2strm, p0, \[x0, z1\.d, uxtw 3\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfd_gather_ext_u64index, svuint64_t,
			 svprfd_gather_u64index (p0, x0, svextw_u64_x (p0, z1), SV_PSTL2STRM),
			 svprfd_gather_index (p0, x0, svextw_x (p0, z1), SV_PSTL2STRM))
