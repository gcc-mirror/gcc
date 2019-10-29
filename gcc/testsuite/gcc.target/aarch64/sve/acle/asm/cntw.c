/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntw_1:
**	cntw	x0
**	ret
*/
PROTO (cntw_1, uint64_t, ()) { return svcntw (); }

/*
** cntw_2:
**	cnth	x0
**	ret
*/
PROTO (cntw_2, uint64_t, ()) { return svcntw () * 2; }

/*
** cntw_3:
**	cntw	x0, all, mul #3
**	ret
*/
PROTO (cntw_3, uint64_t, ()) { return svcntw () * 3; }

/*
** cntw_4:
**	cntb	x0
**	ret
*/
PROTO (cntw_4, uint64_t, ()) { return svcntw () * 4; }

/*
** cntw_8:
**	cntb	x0, all, mul #2
**	ret
*/
PROTO (cntw_8, uint64_t, ()) { return svcntw () * 8; }

/*
** cntw_15:
**	cntw	x0, all, mul #15
**	ret
*/
PROTO (cntw_15, uint64_t, ()) { return svcntw () * 15; }

/*
** cntw_16:
**	cntb	x0, all, mul #4
**	ret
*/
PROTO (cntw_16, uint64_t, ()) { return svcntw () * 16; }

/* Other sequences would be OK.  */
/*
** cntw_17:
**	cntb	x0, all, mul #4
**	incw	x0
**	ret
*/
PROTO (cntw_17, uint64_t, ()) { return svcntw () * 17; }

/*
** cntw_32:
**	cntb	x0, all, mul #8
**	ret
*/
PROTO (cntw_32, uint64_t, ()) { return svcntw () * 32; }

/*
** cntw_64:
**	cntb	x0, all, mul #16
**	ret
*/
PROTO (cntw_64, uint64_t, ()) { return svcntw () * 64; }

/*
** cntw_128:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 8
**	ret
*/
PROTO (cntw_128, uint64_t, ()) { return svcntw () * 128; }

/*
** cntw_m1:
**	cntw	(x[0-9]+)
**	neg	x0, \1
**	ret
*/
PROTO (cntw_m1, uint64_t, ()) { return -svcntw (); }

/*
** cntw_m13:
**	cntw	(x[0-9]+), all, mul #13
**	neg	x0, \1
**	ret
*/
PROTO (cntw_m13, uint64_t, ()) { return -svcntw () * 13; }

/*
** cntw_m15:
**	cntw	(x[0-9]+), all, mul #15
**	neg	x0, \1
**	ret
*/
PROTO (cntw_m15, uint64_t, ()) { return -svcntw () * 15; }

/*
** cntw_m16:
**	cntb	(x[0-9]+), all, mul #4
**	neg	x0, \1
**	ret
*/
PROTO (cntw_m16, uint64_t, ()) { return -svcntw () * 16; }

/* Other sequences would be OK.  */
/*
** cntw_m17:
**	cntb	x0, all, mul #4
**	incw	x0
**	neg	x0, x0
**	ret
*/
PROTO (cntw_m17, uint64_t, ()) { return -svcntw () * 17; }

/*
** incw_1:
**	incw	x0
**	ret
*/
PROTO (incw_1, uint64_t, (uint64_t x0)) { return x0 + svcntw (); }

/*
** incw_2:
**	inch	x0
**	ret
*/
PROTO (incw_2, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 2; }

/*
** incw_3:
**	incw	x0, all, mul #3
**	ret
*/
PROTO (incw_3, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 3; }

/*
** incw_4:
**	incb	x0
**	ret
*/
PROTO (incw_4, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 4; }

/*
** incw_7:
**	incw	x0, all, mul #7
**	ret
*/
PROTO (incw_7, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 7; }

/*
** incw_8:
**	incb	x0, all, mul #2
**	ret
*/
PROTO (incw_8, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 8; }

/*
** incw_9:
**	incw	x0, all, mul #9
**	ret
*/
PROTO (incw_9, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 9; }

/*
** incw_15:
**	incw	x0, all, mul #15
**	ret
*/
PROTO (incw_15, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 15; }

/*
** incw_16:
**	incb	x0, all, mul #4
**	ret
*/
PROTO (incw_16, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 16; }

/*
** incw_18:
**	inch	x0, all, mul #9
**	ret
*/
PROTO (incw_18, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 18; }

/*
** incw_30:
**	inch	x0, all, mul #15
**	ret
*/
PROTO (incw_30, uint64_t, (uint64_t x0)) { return x0 + svcntw () * 30; }

/*
** decw_1:
**	decw	x0
**	ret
*/
PROTO (decw_1, uint64_t, (uint64_t x0)) { return x0 - svcntw (); }

/*
** decw_2:
**	dech	x0
**	ret
*/
PROTO (decw_2, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 2; }

/*
** decw_3:
**	decw	x0, all, mul #3
**	ret
*/
PROTO (decw_3, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 3; }

/*
** decw_4:
**	decb	x0
**	ret
*/
PROTO (decw_4, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 4; }

/*
** decw_7:
**	decw	x0, all, mul #7
**	ret
*/
PROTO (decw_7, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 7; }

/*
** decw_8:
**	decb	x0, all, mul #2
**	ret
*/
PROTO (decw_8, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 8; }

/*
** decw_9:
**	decw	x0, all, mul #9
**	ret
*/
PROTO (decw_9, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 9; }

/*
** decw_15:
**	decw	x0, all, mul #15
**	ret
*/
PROTO (decw_15, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 15; }

/*
** decw_16:
**	decb	x0, all, mul #4
**	ret
*/
PROTO (decw_16, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 16; }

/*
** decw_18:
**	dech	x0, all, mul #9
**	ret
*/
PROTO (decw_18, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 18; }

/*
** decw_30:
**	dech	x0, all, mul #15
**	ret
*/
PROTO (decw_30, uint64_t, (uint64_t x0)) { return x0 - svcntw () * 30; }
