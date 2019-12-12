/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntb_1:
**	cntb	x0
**	ret
*/
PROTO (cntb_1, uint64_t, ()) { return svcntb (); }

/*
** cntb_2:
**	cntb	x0, all, mul #2
**	ret
*/
PROTO (cntb_2, uint64_t, ()) { return svcntb () * 2; }

/*
** cntb_3:
**	cntb	x0, all, mul #3
**	ret
*/
PROTO (cntb_3, uint64_t, ()) { return svcntb () * 3; }

/*
** cntb_4:
**	cntb	x0, all, mul #4
**	ret
*/
PROTO (cntb_4, uint64_t, ()) { return svcntb () * 4; }

/*
** cntb_8:
**	cntb	x0, all, mul #8
**	ret
*/
PROTO (cntb_8, uint64_t, ()) { return svcntb () * 8; }

/*
** cntb_15:
**	cntb	x0, all, mul #15
**	ret
*/
PROTO (cntb_15, uint64_t, ()) { return svcntb () * 15; }

/*
** cntb_16:
**	cntb	x0, all, mul #16
**	ret
*/
PROTO (cntb_16, uint64_t, ()) { return svcntb () * 16; }

/* Other sequences would be OK.  */
/*
** cntb_17:
**	cntb	x0, all, mul #16
**	incb	x0
**	ret
*/
PROTO (cntb_17, uint64_t, ()) { return svcntb () * 17; }

/*
** cntb_32:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 8
**	ret
*/
PROTO (cntb_32, uint64_t, ()) { return svcntb () * 32; }

/* Other sequences would be OK.  */
/*
** cntb_33:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 5
**	incb	x0
**	ret
*/
PROTO (cntb_33, uint64_t, ()) { return svcntb () * 33; }

/*
** cntb_64:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 9
**	ret
*/
PROTO (cntb_64, uint64_t, ()) { return svcntb () * 64; }

/*
** cntb_128:
**	cntd	(x[0-9]+)
**	lsl	x0, \1, 10
**	ret
*/
PROTO (cntb_128, uint64_t, ()) { return svcntb () * 128; }

/* Other sequences would be OK.  */
/*
** cntb_129:
**	cntb	(x[0-9]+)
**	lsl	x0, \1, 7
**	incb	x0
**	ret
*/
PROTO (cntb_129, uint64_t, ()) { return svcntb () * 129; }

/*
** cntb_m1:
**	cntb	(x[0-9]+)
**	neg	x0, \1
**	ret
*/
PROTO (cntb_m1, uint64_t, ()) { return -svcntb (); }

/*
** cntb_m13:
**	cntb	(x[0-9]+), all, mul #13
**	neg	x0, \1
**	ret
*/
PROTO (cntb_m13, uint64_t, ()) { return -svcntb () * 13; }

/*
** cntb_m15:
**	cntb	(x[0-9]+), all, mul #15
**	neg	x0, \1
**	ret
*/
PROTO (cntb_m15, uint64_t, ()) { return -svcntb () * 15; }

/*
** cntb_m16:
**	cntb	(x[0-9]+), all, mul #16
**	neg	x0, \1
**	ret
*/
PROTO (cntb_m16, uint64_t, ()) { return -svcntb () * 16; }

/* Other sequences would be OK.  */
/*
** cntb_m17:
**	cntb	x0, all, mul #16
**	incb	x0
**	neg	x0, x0
**	ret
*/
PROTO (cntb_m17, uint64_t, ()) { return -svcntb () * 17; }

/*
** incb_1:
**	incb	x0
**	ret
*/
PROTO (incb_1, uint64_t, (uint64_t x0)) { return x0 + svcntb (); }

/*
** incb_2:
**	incb	x0, all, mul #2
**	ret
*/
PROTO (incb_2, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 2; }

/*
** incb_3:
**	incb	x0, all, mul #3
**	ret
*/
PROTO (incb_3, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 3; }

/*
** incb_4:
**	incb	x0, all, mul #4
**	ret
*/
PROTO (incb_4, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 4; }

/*
** incb_8:
**	incb	x0, all, mul #8
**	ret
*/
PROTO (incb_8, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 8; }

/*
** incb_15:
**	incb	x0, all, mul #15
**	ret
*/
PROTO (incb_15, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 15; }

/*
** incb_16:
**	incb	x0, all, mul #16
**	ret
*/
PROTO (incb_16, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 16; }

/*
** incb_17:
**	addvl	x0, x0, #17
**	ret
*/
PROTO (incb_17, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 17; }

/*
** incb_31:
**	addvl	x0, x0, #31
**	ret
*/
PROTO (incb_31, uint64_t, (uint64_t x0)) { return x0 + svcntb () * 31; }

/*
** decb_1:
**	decb	x0
**	ret
*/
PROTO (decb_1, uint64_t, (uint64_t x0)) { return x0 - svcntb (); }

/*
** decb_2:
**	decb	x0, all, mul #2
**	ret
*/
PROTO (decb_2, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 2; }

/*
** decb_3:
**	decb	x0, all, mul #3
**	ret
*/
PROTO (decb_3, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 3; }

/*
** decb_4:
**	decb	x0, all, mul #4
**	ret
*/
PROTO (decb_4, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 4; }

/*
** decb_8:
**	decb	x0, all, mul #8
**	ret
*/
PROTO (decb_8, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 8; }

/*
** decb_15:
**	decb	x0, all, mul #15
**	ret
*/
PROTO (decb_15, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 15; }

/*
** decb_16:
**	decb	x0, all, mul #16
**	ret
*/
PROTO (decb_16, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 16; }

/*
** decb_17:
**	addvl	x0, x0, #-17
**	ret
*/
PROTO (decb_17, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 17; }

/*
** decb_31:
**	addvl	x0, x0, #-31
**	ret
*/
PROTO (decb_31, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 31; }

/*
** decb_32:
**	addvl	x0, x0, #-32
**	ret
*/
PROTO (decb_32, uint64_t, (uint64_t x0)) { return x0 - svcntb () * 32; }
