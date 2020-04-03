/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfw_base:
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_base, uint8_t,
	       svprfw (p0, x0, SV_PLDL1KEEP),
	       svprfw (p0, x0, SV_PLDL1KEEP))

/*
** prfw_u8_index:
**	add	(x[0-9+]), (x0, x1|x1, x0)
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_u8_index, uint8_t,
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfw_u8_1:
**	add	(x[0-9+]), x0, #?1
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u8_1, uint8_t,
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfw_u16_index:
**	add	(x[0-9+]), x0, x1, lsl #?1
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u16_index, uint16_t,
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfw_u16_1:
**	add	(x[0-9+]), x0, #?2
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u16_1, uint16_t,
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfw_u32_index:
**	prfw	pldl1keep, p0, \[x0, x1, lsl #?2\]
**	ret
*/
TEST_PREFETCH (prfw_u32_index, uint32_t,
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfw_u32_1:
**	add	(x[0-9+]), x0, #?4
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u32_1, uint32_t,
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfw_u64_index:
**	add	(x[0-9+]), x0, x1, lsl #?3
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u64_index, uint64_t,
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfw_u64_1:
**	add	(x[0-9+]), x0, #?8
**	prfw	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfw_u64_1, uint64_t,
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfw (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfw_pldl1strm:
**	prfw	pldl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pldl1strm, uint8_t,
	       svprfw (p0, x0, SV_PLDL1STRM),
	       svprfw (p0, x0, SV_PLDL1STRM))

/*
** prfw_pldl2keep:
**	prfw	pldl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pldl2keep, uint8_t,
	       svprfw (p0, x0, SV_PLDL2KEEP),
	       svprfw (p0, x0, SV_PLDL2KEEP))

/*
** prfw_pldl2strm:
**	prfw	pldl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pldl2strm, uint8_t,
	       svprfw (p0, x0, SV_PLDL2STRM),
	       svprfw (p0, x0, SV_PLDL2STRM))

/*
** prfw_pldl3keep:
**	prfw	pldl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pldl3keep, uint8_t,
	       svprfw (p0, x0, SV_PLDL3KEEP),
	       svprfw (p0, x0, SV_PLDL3KEEP))

/*
** prfw_pldl3strm:
**	prfw	pldl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pldl3strm, uint8_t,
	       svprfw (p0, x0, SV_PLDL3STRM),
	       svprfw (p0, x0, SV_PLDL3STRM))

/*
** prfw_pstl1keep:
**	prfw	pstl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl1keep, uint8_t,
	       svprfw (p0, x0, SV_PSTL1KEEP),
	       svprfw (p0, x0, SV_PSTL1KEEP))

/*
** prfw_pstl1strm:
**	prfw	pstl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl1strm, uint8_t,
	       svprfw (p0, x0, SV_PSTL1STRM),
	       svprfw (p0, x0, SV_PSTL1STRM))

/*
** prfw_pstl2keep:
**	prfw	pstl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl2keep, uint8_t,
	       svprfw (p0, x0, SV_PSTL2KEEP),
	       svprfw (p0, x0, SV_PSTL2KEEP))

/*
** prfw_pstl2strm:
**	prfw	pstl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl2strm, uint8_t,
	       svprfw (p0, x0, SV_PSTL2STRM),
	       svprfw (p0, x0, SV_PSTL2STRM))

/*
** prfw_pstl3keep:
**	prfw	pstl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl3keep, uint8_t,
	       svprfw (p0, x0, SV_PSTL3KEEP),
	       svprfw (p0, x0, SV_PSTL3KEEP))

/*
** prfw_pstl3strm:
**	prfw	pstl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_pstl3strm, uint8_t,
	       svprfw (p0, x0, SV_PSTL3STRM),
	       svprfw (p0, x0, SV_PSTL3STRM))

/*
** prfw_vnum_0:
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_vnum_0, uint8_t,
	       svprfw_vnum (p0, x0, 0, SV_PLDL1KEEP),
	       svprfw_vnum (p0, x0, 0, SV_PLDL1KEEP))

/*
** prfw_vnum_1:
**	incb	x0
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_vnum_1, uint16_t,
	       svprfw_vnum (p0, x0, 1, SV_PLDL1KEEP),
	       svprfw_vnum (p0, x0, 1, SV_PLDL1KEEP))

/*
** prfw_vnum_2:
**	incb	x0, all, mul #2
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_vnum_2, uint32_t,
	       svprfw_vnum (p0, x0, 2, SV_PLDL1KEEP),
	       svprfw_vnum (p0, x0, 2, SV_PLDL1KEEP))

/*
** prfw_vnum_3:
**	incb	x0, all, mul #3
**	prfw	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfw_vnum_3, uint64_t,
	       svprfw_vnum (p0, x0, 3, SV_PLDL1KEEP),
	       svprfw_vnum (p0, x0, 3, SV_PLDL1KEEP))

/*
** prfw_vnum_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	prfw	pldl1keep, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	prfw	zldl1keep, p0, \[x0, \3\]
** )
**	ret
*/
TEST_PREFETCH (prfw_vnum_x1, uint64_t,
	       svprfw_vnum (p0, x0, x1, SV_PLDL1KEEP),
	       svprfw_vnum (p0, x0, x1, SV_PLDL1KEEP))
