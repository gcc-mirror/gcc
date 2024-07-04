/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfb_gather_u32base:
**	prfb	pldl1keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_u32base, svuint32_t,
			 svprfb_gather_u32base (p0, z0, SV_PLDL1KEEP),
			 svprfb_gather (p0, z0, SV_PLDL1KEEP))

/*
** prfb_gather_u64base:
**	prfb	pldl1strm, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_u64base, svuint64_t,
			 svprfb_gather_u64base (p0, z0, SV_PLDL1STRM),
			 svprfb_gather (p0, z0, SV_PLDL1STRM))

/*
** prfb_gather_x0_u32base_offset:
**	prfb	pldl2keep, p0, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_x0_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, x0, SV_PLDL2KEEP),
			 svprfb_gather_offset (p0, z0, x0, SV_PLDL2KEEP))

/*
** prfb_gather_m1_u32base_offset:
**	mov	(x[0-9]+), #?-1
**	prfb	pldl2strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_m1_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, -1, SV_PLDL2STRM),
			 svprfb_gather_offset (p0, z0, -1, SV_PLDL2STRM))

/*
** prfb_gather_0_u32base_offset:
**	prfb	pldl3keep, p0, \[z0\.s\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_0_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, 0, SV_PLDL3KEEP),
			 svprfb_gather_offset (p0, z0, 0, SV_PLDL3KEEP))

/*
** prfb_gather_5_u32base_offset:
**	prfb	pldl3strm, p0, \[z0\.s, #5\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_5_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, 5, SV_PLDL3STRM),
			 svprfb_gather_offset (p0, z0, 5, SV_PLDL3STRM))

/*
** prfb_gather_31_u32base_offset:
**	prfb	pstl1keep, p0, \[z0\.s, #31\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_31_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, 31, SV_PSTL1KEEP),
			 svprfb_gather_offset (p0, z0, 31, SV_PSTL1KEEP))

/*
** prfb_gather_32_u32base_offset:
**	mov	(x[0-9]+), #?32
**	prfb	pstl1strm, p0, \[\1, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_32_u32base_offset, svuint32_t,
			 svprfb_gather_u32base_offset (p0, z0, 32, SV_PSTL1STRM),
			 svprfb_gather_offset (p0, z0, 32, SV_PSTL1STRM))

/*
** prfb_gather_x0_u64base_offset:
**	prfb	pstl2keep, p0, \[x0, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_x0_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, x0, SV_PSTL2KEEP),
			 svprfb_gather_offset (p0, z0, x0, SV_PSTL2KEEP))

/*
** prfb_gather_m1_u64base_offset:
**	mov	(x[0-9]+), #?-1
**	prfb	pstl2strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_m1_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, -1, SV_PSTL2STRM),
			 svprfb_gather_offset (p0, z0, -1, SV_PSTL2STRM))

/*
** prfb_gather_0_u64base_offset:
**	prfb	pstl3keep, p0, \[z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_0_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, 0, SV_PSTL3KEEP),
			 svprfb_gather_offset (p0, z0, 0, SV_PSTL3KEEP))

/*
** prfb_gather_5_u64base_offset:
**	prfb	pstl3strm, p0, \[z0\.d, #5\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_5_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, 5, SV_PSTL3STRM),
			 svprfb_gather_offset (p0, z0, 5, SV_PSTL3STRM))

/*
** prfb_gather_31_u64base_offset:
**	prfb	pldl1keep, p0, \[z0\.d, #31\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_31_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, 31, SV_PLDL1KEEP),
			 svprfb_gather_offset (p0, z0, 31, SV_PLDL1KEEP))

/*
** prfb_gather_32_u64base_offset:
**	mov	(x[0-9]+), #?32
**	prfb	pldl1strm, p0, \[\1, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_ZS (prfb_gather_32_u64base_offset, svuint64_t,
			 svprfb_gather_u64base_offset (p0, z0, 32, SV_PLDL1STRM),
			 svprfb_gather_offset (p0, z0, 32, SV_PLDL1STRM))

/*
** prfb_gather_x0_s32offset:
**	prfb	pldl2keep, p0, \[x0, z0\.s, sxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_x0_s32offset, svint32_t,
			 svprfb_gather_s32offset (p0, x0, z0, SV_PLDL2KEEP),
			 svprfb_gather_offset (p0, x0, z0, SV_PLDL2KEEP))

/*
** prfb_gather_s32offset:
**	prfb	pldl2strm, p0, \[x0, z1\.s, sxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_s32offset, svint32_t,
			 svprfb_gather_s32offset (p0, x0, z1, SV_PLDL2STRM),
			 svprfb_gather_offset (p0, x0, z1, SV_PLDL2STRM))

/*
** prfb_gather_x0_u32offset:
**	prfb	pldl3keep, p0, \[x0, z0\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_x0_u32offset, svuint32_t,
			 svprfb_gather_u32offset (p0, x0, z0, SV_PLDL3KEEP),
			 svprfb_gather_offset (p0, x0, z0, SV_PLDL3KEEP))

/*
** prfb_gather_u32offset:
**	prfb	pldl3strm, p0, \[x0, z1\.s, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_u32offset, svuint32_t,
			 svprfb_gather_u32offset (p0, x0, z1, SV_PLDL3STRM),
			 svprfb_gather_offset (p0, x0, z1, SV_PLDL3STRM))

/*
** prfb_gather_x0_s64offset:
**	prfb	pstl1keep, p0, \[x0, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_x0_s64offset, svint64_t,
			 svprfb_gather_s64offset (p0, x0, z0, SV_PSTL1KEEP),
			 svprfb_gather_offset (p0, x0, z0, SV_PSTL1KEEP))

/*
** prfb_gather_s64offset:
**	prfb	pstl1strm, p0, \[x0, z1\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_s64offset, svint64_t,
			 svprfb_gather_s64offset (p0, x0, z1, SV_PSTL1STRM),
			 svprfb_gather_offset (p0, x0, z1, SV_PSTL1STRM))

/*
** prfb_gather_ext_s64offset:
**	prfb	pstl1strm, p0, \[x0, z1\.d, sxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_ext_s64offset, svint64_t,
			 svprfb_gather_s64offset (p0, x0, svextw_s64_x (p0, z1), SV_PSTL1STRM),
			 svprfb_gather_offset (p0, x0, svextw_x (p0, z1), SV_PSTL1STRM))

/*
** prfb_gather_x0_u64offset:
**	prfb	pstl2keep, p0, \[x0, z0\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_x0_u64offset, svuint64_t,
			 svprfb_gather_u64offset (p0, x0, z0, SV_PSTL2KEEP),
			 svprfb_gather_offset (p0, x0, z0, SV_PSTL2KEEP))

/*
** prfb_gather_u64offset:
**	prfb	pstl2strm, p0, \[x0, z1\.d\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_u64offset, svuint64_t,
			 svprfb_gather_u64offset (p0, x0, z1, SV_PSTL2STRM),
			 svprfb_gather_offset (p0, x0, z1, SV_PSTL2STRM))

/*
** prfb_gather_ext_u64offset:
**	prfb	pstl2strm, p0, \[x0, z1\.d, uxtw\]
**	ret
*/
TEST_PREFETCH_GATHER_SZ (prfb_gather_ext_u64offset, svuint64_t,
			 svprfb_gather_u64offset (p0, x0, svextw_u64_x (p0, z1), SV_PSTL2STRM),
			 svprfb_gather_offset (p0, x0, svextw_x (p0, z1), SV_PSTL2STRM))
