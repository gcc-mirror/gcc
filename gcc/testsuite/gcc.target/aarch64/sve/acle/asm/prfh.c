/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfh_base:
**	prfh	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_base, uint8_t,
	       svprfh (p0, x0, SV_PLDL1KEEP),
	       svprfh (p0, x0, SV_PLDL1KEEP))

/*
** prfh_u8_index:
**	add	(x[0-9+]), (x0, x1|x1, x0)
**	prfh	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_u8_index, uint8_t,
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfh_u8_1:
**	add	(x[0-9+]), x0, #?1
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u8_1, uint8_t,
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfh_u16_index:
**	prfh	pldl1keep, p0, \[x0, x1, lsl #?1\]
**	ret
*/
TEST_PREFETCH (prfh_u16_index, uint16_t,
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfh_u16_1:
**	add	(x[0-9+]), x0, #?2
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u16_1, uint16_t,
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfh_u32_index:
**	add	(x[0-9+]), x0, x1, lsl #?2
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u32_index, uint32_t,
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfh_u32_1:
**	add	(x[0-9+]), x0, #?4
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u32_1, uint32_t,
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfh_u64_index:
**	add	(x[0-9+]), x0, x1, lsl #?3
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u64_index, uint64_t,
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfh_u64_1:
**	add	(x[0-9+]), x0, #?8
**	prfh	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfh_u64_1, uint64_t,
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfh (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfh_pldl1strm:
**	prfh	pldl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pldl1strm, uint8_t,
	       svprfh (p0, x0, SV_PLDL1STRM),
	       svprfh (p0, x0, SV_PLDL1STRM))

/*
** prfh_pldl2keep:
**	prfh	pldl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pldl2keep, uint8_t,
	       svprfh (p0, x0, SV_PLDL2KEEP),
	       svprfh (p0, x0, SV_PLDL2KEEP))

/*
** prfh_pldl2strm:
**	prfh	pldl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pldl2strm, uint8_t,
	       svprfh (p0, x0, SV_PLDL2STRM),
	       svprfh (p0, x0, SV_PLDL2STRM))

/*
** prfh_pldl3keep:
**	prfh	pldl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pldl3keep, uint8_t,
	       svprfh (p0, x0, SV_PLDL3KEEP),
	       svprfh (p0, x0, SV_PLDL3KEEP))

/*
** prfh_pldl3strm:
**	prfh	pldl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pldl3strm, uint8_t,
	       svprfh (p0, x0, SV_PLDL3STRM),
	       svprfh (p0, x0, SV_PLDL3STRM))

/*
** prfh_pstl1keep:
**	prfh	pstl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl1keep, uint8_t,
	       svprfh (p0, x0, SV_PSTL1KEEP),
	       svprfh (p0, x0, SV_PSTL1KEEP))

/*
** prfh_pstl1strm:
**	prfh	pstl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl1strm, uint8_t,
	       svprfh (p0, x0, SV_PSTL1STRM),
	       svprfh (p0, x0, SV_PSTL1STRM))

/*
** prfh_pstl2keep:
**	prfh	pstl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl2keep, uint8_t,
	       svprfh (p0, x0, SV_PSTL2KEEP),
	       svprfh (p0, x0, SV_PSTL2KEEP))

/*
** prfh_pstl2strm:
**	prfh	pstl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl2strm, uint8_t,
	       svprfh (p0, x0, SV_PSTL2STRM),
	       svprfh (p0, x0, SV_PSTL2STRM))

/*
** prfh_pstl3keep:
**	prfh	pstl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl3keep, uint8_t,
	       svprfh (p0, x0, SV_PSTL3KEEP),
	       svprfh (p0, x0, SV_PSTL3KEEP))

/*
** prfh_pstl3strm:
**	prfh	pstl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_pstl3strm, uint8_t,
	       svprfh (p0, x0, SV_PSTL3STRM),
	       svprfh (p0, x0, SV_PSTL3STRM))

/*
** prfh_vnum_0:
**	prfh	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_0, uint8_t,
	       svprfh_vnum (p0, x0, 0, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, 0, SV_PLDL1KEEP))

/*
** prfh_vnum_1:
**	prfh	pldl1keep, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_1, uint16_t,
	       svprfh_vnum (p0, x0, 1, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, 1, SV_PLDL1KEEP))

/*
** prfh_vnum_31:
**	prfh	pldl1keep, p0, \[x0, #31, mul vl\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_31, uint16_t,
	       svprfh_vnum (p0, x0, 31, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, 31, SV_PLDL1KEEP))

/*
** prfh_vnum_32:
**	cntb	(x[0-9]+)
**	lsl	(x[0-9]+), \1, #?5
**	add	(x[0-9]+), (\2, x0|x0, \2)
**	prfh	pldl1keep, p0, \[\3\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_32, uint16_t,
	       svprfh_vnum (p0, x0, 32, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, 32, SV_PLDL1KEEP))

/*
** prfh_vnum_m32:
**	prfh	pldl1keep, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_m32, uint16_t,
	       svprfh_vnum (p0, x0, -32, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, -32, SV_PLDL1KEEP))

/*
** prfh_vnum_m33:
**	...
**	prfh	pldl1keep, p0, \[x[0-9]+\]
**	ret
*/
TEST_PREFETCH (prfh_vnum_m33, uint16_t,
	       svprfh_vnum (p0, x0, -33, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, -33, SV_PLDL1KEEP))

/*
** prfh_vnum_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	prfh	pldl1keep, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	prfh	zldl1keep, p0, \[x0, \3\]
** )
**	ret
*/
TEST_PREFETCH (prfh_vnum_x1, uint64_t,
	       svprfh_vnum (p0, x0, x1, SV_PLDL1KEEP),
	       svprfh_vnum (p0, x0, x1, SV_PLDL1KEEP))
