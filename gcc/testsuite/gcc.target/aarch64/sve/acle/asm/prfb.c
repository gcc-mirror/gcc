/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** prfb_base:
**	prfb	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_base, uint8_t,
	       svprfb (p0, x0, SV_PLDL1KEEP),
	       svprfb (p0, x0, SV_PLDL1KEEP))

/*
** prfb_u8_index:
**	prfb	pldl1keep, p0, \[x0, x1\]
**	ret
*/
TEST_PREFETCH (prfb_u8_index, uint8_t,
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfb_u8_1:
**	add	(x[0-9+]), x0, #?1
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u8_1, uint8_t,
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfb_u16_index:
**	add	(x[0-9+]), x0, x1, lsl #?1
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u16_index, uint16_t,
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfb_u16_1:
**	add	(x[0-9+]), x0, #?2
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u16_1, uint16_t,
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfb_u32_index:
**	add	(x[0-9+]), x0, x1, lsl #?2
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u32_index, uint32_t,
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfb_u32_1:
**	add	(x[0-9+]), x0, #?4
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u32_1, uint32_t,
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfb_u64_index:
**	add	(x[0-9+]), x0, x1, lsl #?3
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u64_index, uint64_t,
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfb_u64_1:
**	add	(x[0-9+]), x0, #?8
**	prfb	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfb_u64_1, uint64_t,
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfb (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfb_pldl1strm:
**	prfb	pldl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pldl1strm, uint8_t,
	       svprfb (p0, x0, SV_PLDL1STRM),
	       svprfb (p0, x0, SV_PLDL1STRM))

/*
** prfb_pldl2keep:
**	prfb	pldl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pldl2keep, uint8_t,
	       svprfb (p0, x0, SV_PLDL2KEEP),
	       svprfb (p0, x0, SV_PLDL2KEEP))

/*
** prfb_pldl2strm:
**	prfb	pldl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pldl2strm, uint8_t,
	       svprfb (p0, x0, SV_PLDL2STRM),
	       svprfb (p0, x0, SV_PLDL2STRM))

/*
** prfb_pldl3keep:
**	prfb	pldl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pldl3keep, uint8_t,
	       svprfb (p0, x0, SV_PLDL3KEEP),
	       svprfb (p0, x0, SV_PLDL3KEEP))

/*
** prfb_pldl3strm:
**	prfb	pldl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pldl3strm, uint8_t,
	       svprfb (p0, x0, SV_PLDL3STRM),
	       svprfb (p0, x0, SV_PLDL3STRM))

/*
** prfb_pstl1keep:
**	prfb	pstl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl1keep, uint8_t,
	       svprfb (p0, x0, SV_PSTL1KEEP),
	       svprfb (p0, x0, SV_PSTL1KEEP))

/*
** prfb_pstl1strm:
**	prfb	pstl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl1strm, uint8_t,
	       svprfb (p0, x0, SV_PSTL1STRM),
	       svprfb (p0, x0, SV_PSTL1STRM))

/*
** prfb_pstl2keep:
**	prfb	pstl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl2keep, uint8_t,
	       svprfb (p0, x0, SV_PSTL2KEEP),
	       svprfb (p0, x0, SV_PSTL2KEEP))

/*
** prfb_pstl2strm:
**	prfb	pstl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl2strm, uint8_t,
	       svprfb (p0, x0, SV_PSTL2STRM),
	       svprfb (p0, x0, SV_PSTL2STRM))

/*
** prfb_pstl3keep:
**	prfb	pstl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl3keep, uint8_t,
	       svprfb (p0, x0, SV_PSTL3KEEP),
	       svprfb (p0, x0, SV_PSTL3KEEP))

/*
** prfb_pstl3strm:
**	prfb	pstl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_pstl3strm, uint8_t,
	       svprfb (p0, x0, SV_PSTL3STRM),
	       svprfb (p0, x0, SV_PSTL3STRM))

/*
** prfb_vnum_0:
**	prfb	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_vnum_0, uint8_t,
	       svprfb_vnum (p0, x0, 0, SV_PLDL1KEEP),
	       svprfb_vnum (p0, x0, 0, SV_PLDL1KEEP))

/*
** prfb_vnum_1:
**	incb	x0
**	prfb	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_vnum_1, uint16_t,
	       svprfb_vnum (p0, x0, 1, SV_PLDL1KEEP),
	       svprfb_vnum (p0, x0, 1, SV_PLDL1KEEP))

/*
** prfb_vnum_2:
**	incb	x0, all, mul #2
**	prfb	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_vnum_2, uint32_t,
	       svprfb_vnum (p0, x0, 2, SV_PLDL1KEEP),
	       svprfb_vnum (p0, x0, 2, SV_PLDL1KEEP))

/*
** prfb_vnum_3:
**	incb	x0, all, mul #3
**	prfb	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfb_vnum_3, uint64_t,
	       svprfb_vnum (p0, x0, 3, SV_PLDL1KEEP),
	       svprfb_vnum (p0, x0, 3, SV_PLDL1KEEP))

/*
** prfb_vnum_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	prfb	pldl1keep, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	prfb	zldl1keep, p0, \[x0, \3\]
** )
**	ret
*/
TEST_PREFETCH (prfb_vnum_x1, uint64_t,
	       svprfb_vnum (p0, x0, x1, SV_PLDL1KEEP),
	       svprfb_vnum (p0, x0, x1, SV_PLDL1KEEP))
