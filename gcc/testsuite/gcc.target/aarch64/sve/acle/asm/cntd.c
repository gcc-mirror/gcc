/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntd_1:
**	cntd	x0
**	ret
*/
PROTO (cntd_1, uint64_t, ()) { return svcntd (); }

/*
** cntd_2:
**	cntw	x0
**	ret
*/
PROTO (cntd_2, uint64_t, ()) { return svcntd () * 2; }

/*
** cntd_3:
**	cntd	x0, all, mul #3
**	ret
*/
PROTO (cntd_3, uint64_t, ()) { return svcntd () * 3; }

/*
** cntd_4:
**	cnth	x0
**	ret
*/
PROTO (cntd_4, uint64_t, ()) { return svcntd () * 4; }

/*
** cntd_8:
**	cntb	x0
**	ret
*/
PROTO (cntd_8, uint64_t, ()) { return svcntd () * 8; }

/*
** cntd_15:
**	cntd	x0, all, mul #15
**	ret
*/
PROTO (cntd_15, uint64_t, ()) { return svcntd () * 15; }

/*
** cntd_16:
**	cntb	x0, all, mul #2
**	ret
*/
PROTO (cntd_16, uint64_t, ()) { return svcntd () * 16; }

/* Other sequences would be OK.  */
/*
** cntd_17:
**	rdvl	(x[0-9]+), #17
**	asr	x0, \1, 3
**	ret
*/
PROTO (cntd_17, uint64_t, ()) { return svcntd () * 17; }

/*
** cntd_32:
**	cntb	x0, all, mul #4
**	ret
*/
PROTO (cntd_32, uint64_t, ()) { return svcntd () * 32; }

/*
** cntd_64:
**	cntb	x0, all, mul #8
**	ret
*/
PROTO (cntd_64, uint64_t, ()) { return svcntd () * 64; }

/*
** cntd_128:
**	cntb	x0, all, mul #16
**	ret
*/
PROTO (cntd_128, uint64_t, ()) { return svcntd () * 128; }

/*
** cntd_m1:
**	cntd	(x[0-9]+)
**	neg	x0, \1
**	ret
*/
PROTO (cntd_m1, uint64_t, ()) { return -svcntd (); }

/*
** cntd_m13:
**	cntd	(x[0-9]+), all, mul #13
**	neg	x0, \1
**	ret
*/
PROTO (cntd_m13, uint64_t, ()) { return -svcntd () * 13; }

/*
** cntd_m15:
**	cntd	(x[0-9]+), all, mul #15
**	neg	x0, \1
**	ret
*/
PROTO (cntd_m15, uint64_t, ()) { return -svcntd () * 15; }

/*
** cntd_m16:
**	rdvl	x0, #-2
**	ret
*/
PROTO (cntd_m16, uint64_t, ()) { return -svcntd () * 16; }

/* Other sequences would be OK.  */
/*
** cntd_m17:
**	rdvl	(x[0-9]+), #-17
**	asr	x0, \1, 3
**	ret
*/
PROTO (cntd_m17, uint64_t, ()) { return -svcntd () * 17; }

/*
** incd_1:
**	incd	x0
**	ret
*/
PROTO (incd_1, uint64_t, (uint64_t x0)) { return x0 + svcntd (); }

/*
** incd_2:
**	incw	x0
**	ret
*/
PROTO (incd_2, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 2; }

/*
** incd_3:
**	incd	x0, all, mul #3
**	ret
*/
PROTO (incd_3, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 3; }

/*
** incd_4:
**	inch	x0
**	ret
*/
PROTO (incd_4, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 4; }

/*
** incd_7:
**	incd	x0, all, mul #7
**	ret
*/
PROTO (incd_7, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 7; }

/*
** incd_8:
**	incb	x0
**	ret
*/
PROTO (incd_8, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 8; }

/*
** incd_9:
**	incd	x0, all, mul #9
**	ret
*/
PROTO (incd_9, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 9; }

/*
** incd_15:
**	incd	x0, all, mul #15
**	ret
*/
PROTO (incd_15, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 15; }

/*
** incd_16:
**	incb	x0, all, mul #2
**	ret
*/
PROTO (incd_16, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 16; }

/*
** incd_18:
**	incw	x0, all, mul #9
**	ret
*/
PROTO (incd_18, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 18; }

/*
** incd_30:
**	incw	x0, all, mul #15
**	ret
*/
PROTO (incd_30, uint64_t, (uint64_t x0)) { return x0 + svcntd () * 30; }

/*
** decd_1:
**	decd	x0
**	ret
*/
PROTO (decd_1, uint64_t, (uint64_t x0)) { return x0 - svcntd (); }

/*
** decd_2:
**	decw	x0
**	ret
*/
PROTO (decd_2, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 2; }

/*
** decd_3:
**	decd	x0, all, mul #3
**	ret
*/
PROTO (decd_3, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 3; }

/*
** decd_4:
**	dech	x0
**	ret
*/
PROTO (decd_4, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 4; }

/*
** decd_7:
**	decd	x0, all, mul #7
**	ret
*/
PROTO (decd_7, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 7; }

/*
** decd_8:
**	decb	x0
**	ret
*/
PROTO (decd_8, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 8; }

/*
** decd_9:
**	decd	x0, all, mul #9
**	ret
*/
PROTO (decd_9, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 9; }

/*
** decd_15:
**	decd	x0, all, mul #15
**	ret
*/
PROTO (decd_15, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 15; }

/*
** decd_16:
**	decb	x0, all, mul #2
**	ret
*/
PROTO (decd_16, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 16; }

/*
** decd_18:
**	decw	x0, all, mul #9
**	ret
*/
PROTO (decd_18, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 18; }

/*
** decd_30:
**	decw	x0, all, mul #15
**	ret
*/
PROTO (decd_30, uint64_t, (uint64_t x0)) { return x0 - svcntd () * 30; }
