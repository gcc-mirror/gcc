/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnth_1:
**	cnth	x0
**	ret
*/
PROTO (cnth_1, uint64_t, ()) { return svcnth (); }

/*
** cnth_2:
**	cntb	x0
**	ret
*/
PROTO (cnth_2, uint64_t, ()) { return svcnth () * 2; }

/*
** cnth_3:
**	cnth	x0, all, mul #3
**	ret
*/
PROTO (cnth_3, uint64_t, ()) { return svcnth () * 3; }

/*
** cnth_4:
**	cntb	x0, all, mul #2
**	ret
*/
PROTO (cnth_4, uint64_t, ()) { return svcnth () * 4; }

/*
** cnth_8:
**	cntb	x0, all, mul #4
**	ret
*/
PROTO (cnth_8, uint64_t, ()) { return svcnth () * 8; }

/*
** cnth_15:
**	cnth	x0, all, mul #15
**	ret
*/
PROTO (cnth_15, uint64_t, ()) { return svcnth () * 15; }

/*
** cnth_16:
**	cntb	x0, all, mul #8
**	ret
*/
PROTO (cnth_16, uint64_t, ()) { return svcnth () * 16; }

/* Other sequences would be OK.  */
/*
** cnth_17:
**	cntb	x0, all, mul #8
**	inch	x0
**	ret
*/
PROTO (cnth_17, uint64_t, ()) { return svcnth () * 17; }

/*
** cnth_32:
**	cntb	x0, all, mul #16
**	ret
*/
PROTO (cnth_32, uint64_t, ()) { return svcnth () * 32; }

/*
** cnth_64:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 8
**	ret
*/
PROTO (cnth_64, uint64_t, ()) { return svcnth () * 64; }

/*
** cnth_128:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 9
**	ret
*/
PROTO (cnth_128, uint64_t, ()) { return svcnth () * 128; }

/*
** cnth_m1:
**	cnth	(x[0-9]+)
**	neg	x0, \1
**	ret
*/
PROTO (cnth_m1, uint64_t, ()) { return -svcnth (); }

/*
** cnth_m13:
**	cnth	(x[0-9]+), all, mul #13
**	neg	x0, \1
**	ret
*/
PROTO (cnth_m13, uint64_t, ()) { return -svcnth () * 13; }

/*
** cnth_m15:
**	cnth	(x[0-9]+), all, mul #15
**	neg	x0, \1
**	ret
*/
PROTO (cnth_m15, uint64_t, ()) { return -svcnth () * 15; }

/*
** cnth_m16:
**	cntb	(x[0-9]+), all, mul #8
**	neg	x0, \1
**	ret
*/
PROTO (cnth_m16, uint64_t, ()) { return -svcnth () * 16; }

/* Other sequences would be OK.  */
/*
** cnth_m17:
**	cntb	x0, all, mul #8
**	inch	x0
**	neg	x0, x0
**	ret
*/
PROTO (cnth_m17, uint64_t, ()) { return -svcnth () * 17; }

/*
** inch_1:
**	inch	x0
**	ret
*/
PROTO (inch_1, uint64_t, (uint64_t x0)) { return x0 + svcnth (); }

/*
** inch_2:
**	incb	x0
**	ret
*/
PROTO (inch_2, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 2; }

/*
** inch_3:
**	inch	x0, all, mul #3
**	ret
*/
PROTO (inch_3, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 3; }

/*
** inch_4:
**	incb	x0, all, mul #2
**	ret
*/
PROTO (inch_4, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 4; }

/*
** inch_7:
**	inch	x0, all, mul #7
**	ret
*/
PROTO (inch_7, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 7; }

/*
** inch_8:
**	incb	x0, all, mul #4
**	ret
*/
PROTO (inch_8, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 8; }

/*
** inch_9:
**	inch	x0, all, mul #9
**	ret
*/
PROTO (inch_9, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 9; }

/*
** inch_15:
**	inch	x0, all, mul #15
**	ret
*/
PROTO (inch_15, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 15; }

/*
** inch_16:
**	incb	x0, all, mul #8
**	ret
*/
PROTO (inch_16, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 16; }

/*
** inch_18:
**	incb	x0, all, mul #9
**	ret
*/
PROTO (inch_18, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 18; }

/*
** inch_30:
**	incb	x0, all, mul #15
**	ret
*/
PROTO (inch_30, uint64_t, (uint64_t x0)) { return x0 + svcnth () * 30; }

/*
** dech_1:
**	dech	x0
**	ret
*/
PROTO (dech_1, uint64_t, (uint64_t x0)) { return x0 - svcnth (); }

/*
** dech_2:
**	decb	x0
**	ret
*/
PROTO (dech_2, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 2; }

/*
** dech_3:
**	dech	x0, all, mul #3
**	ret
*/
PROTO (dech_3, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 3; }

/*
** dech_4:
**	decb	x0, all, mul #2
**	ret
*/
PROTO (dech_4, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 4; }

/*
** dech_7:
**	dech	x0, all, mul #7
**	ret
*/
PROTO (dech_7, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 7; }

/*
** dech_8:
**	decb	x0, all, mul #4
**	ret
*/
PROTO (dech_8, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 8; }

/*
** dech_9:
**	dech	x0, all, mul #9
**	ret
*/
PROTO (dech_9, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 9; }

/*
** dech_15:
**	dech	x0, all, mul #15
**	ret
*/
PROTO (dech_15, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 15; }

/*
** dech_16:
**	decb	x0, all, mul #8
**	ret
*/
PROTO (dech_16, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 16; }

/*
** dech_18:
**	decb	x0, all, mul #9
**	ret
*/
PROTO (dech_18, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 18; }

/*
** dech_30:
**	decb	x0, all, mul #15
**	ret
*/
PROTO (dech_30, uint64_t, (uint64_t x0)) { return x0 - svcnth () * 30; }
