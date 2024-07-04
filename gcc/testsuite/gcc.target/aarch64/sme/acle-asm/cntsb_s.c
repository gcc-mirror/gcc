/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define NO_SHARED_ZA
#include "test_sme_acle.h"

/*
** cntb_1:
**	cntb	x0
**	ret
*/
PROTO (cntb_1, uint64_t, ()) { return svcntsb (); }

/*
** cntb_2:
**	cntb	x0, all, mul #2
**	ret
*/
PROTO (cntb_2, uint64_t, ()) { return svcntsb () * 2; }

/*
** cntb_3:
**	cntb	x0, all, mul #3
**	ret
*/
PROTO (cntb_3, uint64_t, ()) { return svcntsb () * 3; }

/*
** cntb_4:
**	cntb	x0, all, mul #4
**	ret
*/
PROTO (cntb_4, uint64_t, ()) { return svcntsb () * 4; }

/*
** cntb_8:
**	cntb	x0, all, mul #8
**	ret
*/
PROTO (cntb_8, uint64_t, ()) { return svcntsb () * 8; }

/*
** cntb_15:
**	cntb	x0, all, mul #15
**	ret
*/
PROTO (cntb_15, uint64_t, ()) { return svcntsb () * 15; }

/*
** cntb_16:
**	cntb	x0, all, mul #16
**	ret
*/
PROTO (cntb_16, uint64_t, ()) { return svcntsb () * 16; }

/*
** cntb_17:
**	rdvl	x0, #17
**	ret
*/
PROTO (cntb_17, uint64_t, ()) { return svcntsb () * 17; }

/*
** cntb_31:
**	rdvl	x0, #31
**	ret
*/
PROTO (cntb_31, uint64_t, ()) { return svcntsb () * 31; }

/*
** cntb_32:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 5
**	ret
*/
PROTO (cntb_32, uint64_t, ()) { return svcntsb () * 32; }

/* Other sequences would be OK.  */
/*
** cntb_33:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 5
**	incb	x0
**	ret
*/
PROTO (cntb_33, uint64_t, ()) { return svcntsb () * 33; }

/*
** cntb_64:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 6
**	ret
*/
PROTO (cntb_64, uint64_t, ()) { return svcntsb () * 64; }

/*
** cntb_128:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 7
**	ret
*/
PROTO (cntb_128, uint64_t, ()) { return svcntsb () * 128; }

/* Other sequences would be OK.  */
/*
** cntb_129:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 7
**	incb	x0
**	ret
*/
PROTO (cntb_129, uint64_t, ()) { return svcntsb () * 129; }

/*
** cntb_m1:
**	rdvl	x0, #-1
**	ret
*/
PROTO (cntb_m1, uint64_t, ()) { return -svcntsb (); }

/*
** cntb_m13:
**	rdvl	x0, #-13
**	ret
*/
PROTO (cntb_m13, uint64_t, ()) { return -svcntsb () * 13; }

/*
** cntb_m15:
**	rdvl	x0, #-15
**	ret
*/
PROTO (cntb_m15, uint64_t, ()) { return -svcntsb () * 15; }

/*
** cntb_m16:
**	rdvl	x0, #-16
**	ret
*/
PROTO (cntb_m16, uint64_t, ()) { return -svcntsb () * 16; }

/*
** cntb_m17:
**	rdvl	x0, #-17
**	ret
*/
PROTO (cntb_m17, uint64_t, ()) { return -svcntsb () * 17; }

/*
** cntb_m32:
**	rdvl	x0, #-32
**	ret
*/
PROTO (cntb_m32, uint64_t, ()) { return -svcntsb () * 32; }

/*
** cntb_m33:
**	rdvl	x0, #-32
**	decb	x0
**	ret
*/
PROTO (cntb_m33, uint64_t, ()) { return -svcntsb () * 33; }

/*
** cntb_m34:
**	rdvl	(x[0-9]+), #-17
**	lsl	x0, \1, #?1
**	ret
*/
PROTO (cntb_m34, uint64_t, ()) { return -svcntsb () * 34; }

/*
** cntb_m64:
**	rdvl	(x[0-9]+), #-1
**	lsl	x0, \1, #?6
**	ret
*/
PROTO (cntb_m64, uint64_t, ()) { return -svcntsb () * 64; }

/*
** incb_1:
**	incb	x0
**	ret
*/
PROTO (incb_1, uint64_t, (uint64_t x0)) { return x0 + svcntsb (); }

/*
** incb_2:
**	incb	x0, all, mul #2
**	ret
*/
PROTO (incb_2, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 2; }

/*
** incb_3:
**	incb	x0, all, mul #3
**	ret
*/
PROTO (incb_3, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 3; }

/*
** incb_4:
**	incb	x0, all, mul #4
**	ret
*/
PROTO (incb_4, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 4; }

/*
** incb_8:
**	incb	x0, all, mul #8
**	ret
*/
PROTO (incb_8, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 8; }

/*
** incb_15:
**	incb	x0, all, mul #15
**	ret
*/
PROTO (incb_15, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 15; }

/*
** incb_16:
**	incb	x0, all, mul #16
**	ret
*/
PROTO (incb_16, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 16; }

/*
** incb_17:
**	addvl	x0, x0, #17
**	ret
*/
PROTO (incb_17, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 17; }

/*
** incb_31:
**	addvl	x0, x0, #31
**	ret
*/
PROTO (incb_31, uint64_t, (uint64_t x0)) { return x0 + svcntsb () * 31; }

/*
** decb_1:
**	decb	x0
**	ret
*/
PROTO (decb_1, uint64_t, (uint64_t x0)) { return x0 - svcntsb (); }

/*
** decb_2:
**	decb	x0, all, mul #2
**	ret
*/
PROTO (decb_2, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 2; }

/*
** decb_3:
**	decb	x0, all, mul #3
**	ret
*/
PROTO (decb_3, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 3; }

/*
** decb_4:
**	decb	x0, all, mul #4
**	ret
*/
PROTO (decb_4, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 4; }

/*
** decb_8:
**	decb	x0, all, mul #8
**	ret
*/
PROTO (decb_8, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 8; }

/*
** decb_15:
**	decb	x0, all, mul #15
**	ret
*/
PROTO (decb_15, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 15; }

/*
** decb_16:
**	decb	x0, all, mul #16
**	ret
*/
PROTO (decb_16, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 16; }

/*
** decb_17:
**	addvl	x0, x0, #-17
**	ret
*/
PROTO (decb_17, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 17; }

/*
** decb_31:
**	addvl	x0, x0, #-31
**	ret
*/
PROTO (decb_31, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 31; }

/*
** decb_32:
**	addvl	x0, x0, #-32
**	ret
*/
PROTO (decb_32, uint64_t, (uint64_t x0)) { return x0 - svcntsb () * 32; }
