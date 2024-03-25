/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** prfd_base:
**	prfd	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_base, uint8_t,
	       svprfd (p0, x0, SV_PLDL1KEEP),
	       svprfd (p0, x0, SV_PLDL1KEEP))

/*
** prfd_u8_index:
**	add	(x[0-9+]), (x0, x1|x1, x0)
**	prfd	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_u8_index, uint8_t,
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfd_u8_1:
**	add	(x[0-9+]), x0, #?1
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u8_1, uint8_t,
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfd_u16_index:
**	add	(x[0-9+]), x0, x1, lsl #?1
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u16_index, uint16_t,
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfd_u16_1:
**	add	(x[0-9+]), x0, #?2
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u16_1, uint16_t,
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfd_u32_index:
**	add	(x[0-9+]), x0, x1, lsl #?2
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u32_index, uint32_t,
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfd_u32_1:
**	add	(x[0-9+]), x0, #?4
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u32_1, uint32_t,
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfd_u64_index:
**	prfd	pldl1keep, p0, \[x0, x1, lsl #?3\]
**	ret
*/
TEST_PREFETCH (prfd_u64_index, uint64_t,
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + x1, SV_PLDL1KEEP))

/*
** prfd_u64_1:
**	add	(x[0-9+]), x0, #?8
**	prfd	pldl1keep, p0, \[\1\]
**	ret
*/
TEST_PREFETCH (prfd_u64_1, uint64_t,
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP),
	       svprfd (p0, x0 + 1, SV_PLDL1KEEP))

/*
** prfd_pldl1strm:
**	prfd	pldl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pldl1strm, uint8_t,
	       svprfd (p0, x0, SV_PLDL1STRM),
	       svprfd (p0, x0, SV_PLDL1STRM))

/*
** prfd_pldl2keep:
**	prfd	pldl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pldl2keep, uint8_t,
	       svprfd (p0, x0, SV_PLDL2KEEP),
	       svprfd (p0, x0, SV_PLDL2KEEP))

/*
** prfd_pldl2strm:
**	prfd	pldl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pldl2strm, uint8_t,
	       svprfd (p0, x0, SV_PLDL2STRM),
	       svprfd (p0, x0, SV_PLDL2STRM))

/*
** prfd_pldl3keep:
**	prfd	pldl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pldl3keep, uint8_t,
	       svprfd (p0, x0, SV_PLDL3KEEP),
	       svprfd (p0, x0, SV_PLDL3KEEP))

/*
** prfd_pldl3strm:
**	prfd	pldl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pldl3strm, uint8_t,
	       svprfd (p0, x0, SV_PLDL3STRM),
	       svprfd (p0, x0, SV_PLDL3STRM))

/*
** prfd_pstl1keep:
**	prfd	pstl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl1keep, uint8_t,
	       svprfd (p0, x0, SV_PSTL1KEEP),
	       svprfd (p0, x0, SV_PSTL1KEEP))

/*
** prfd_pstl1strm:
**	prfd	pstl1strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl1strm, uint8_t,
	       svprfd (p0, x0, SV_PSTL1STRM),
	       svprfd (p0, x0, SV_PSTL1STRM))

/*
** prfd_pstl2keep:
**	prfd	pstl2keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl2keep, uint8_t,
	       svprfd (p0, x0, SV_PSTL2KEEP),
	       svprfd (p0, x0, SV_PSTL2KEEP))

/*
** prfd_pstl2strm:
**	prfd	pstl2strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl2strm, uint8_t,
	       svprfd (p0, x0, SV_PSTL2STRM),
	       svprfd (p0, x0, SV_PSTL2STRM))

/*
** prfd_pstl3keep:
**	prfd	pstl3keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl3keep, uint8_t,
	       svprfd (p0, x0, SV_PSTL3KEEP),
	       svprfd (p0, x0, SV_PSTL3KEEP))

/*
** prfd_pstl3strm:
**	prfd	pstl3strm, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_pstl3strm, uint8_t,
	       svprfd (p0, x0, SV_PSTL3STRM),
	       svprfd (p0, x0, SV_PSTL3STRM))

/*
** prfd_vnum_0:
**	prfd	pldl1keep, p0, \[x0\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_0, uint8_t,
	       svprfd_vnum (p0, x0, 0, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, 0, SV_PLDL1KEEP))

/*
** prfd_vnum_1:
**	prfd	pldl1keep, p0, \[x0, #1, mul vl\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_1, uint16_t,
	       svprfd_vnum (p0, x0, 1, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, 1, SV_PLDL1KEEP))

/*
** prfd_vnum_31:
**	prfd	pldl1keep, p0, \[x0, #31, mul vl\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_31, uint16_t,
	       svprfd_vnum (p0, x0, 31, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, 31, SV_PLDL1KEEP))

/*
** prfd_vnum_32:
**	cntb	(x[0-9]+)
**	lsl	(x[0-9]+), \1, #?5
**	add	(x[0-9]+), (\2, x0|x0, \2)
**	prfd	pldl1keep, p0, \[\3\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_32, uint16_t,
	       svprfd_vnum (p0, x0, 32, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, 32, SV_PLDL1KEEP))

/*
** prfd_vnum_m32:
**	prfd	pldl1keep, p0, \[x0, #-32, mul vl\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_m32, uint16_t,
	       svprfd_vnum (p0, x0, -32, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, -32, SV_PLDL1KEEP))

/*
** prfd_vnum_m33:
**	...
**	prfd	pldl1keep, p0, \[x[0-9]+\]
**	ret
*/
TEST_PREFETCH (prfd_vnum_m33, uint16_t,
	       svprfd_vnum (p0, x0, -33, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, -33, SV_PLDL1KEEP))

/*
** prfd_vnum_x1:
**	cntb	(x[0-9]+)
** (
**	madd	(x[0-9]+), (?:x1, \1|\1, x1), x0
**	prfd	pldl1keep, p0, \[\2\]
** |
**	mul	(x[0-9]+), (?:x1, \1|\1, x1)
**	prfd	zldl1keep, p0, \[x0, \3\]
** )
**	ret
*/
TEST_PREFETCH (prfd_vnum_x1, uint64_t,
	       svprfd_vnum (p0, x0, x1, SV_PLDL1KEEP),
	       svprfd_vnum (p0, x0, x1, SV_PLDL1KEEP))
