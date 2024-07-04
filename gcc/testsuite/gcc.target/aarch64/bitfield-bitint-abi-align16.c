/* { dg-do compile { target bitint } } */
/* { dg-additional-options "-std=c23 -O2 -fno-stack-protector -save-temps -fno-schedule-insns -fno-schedule-insns2 -fno-late-combine-instructions" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define ALIGN 16
#include "bitfield-bitint-abi.h"

// f1-f16 are all the same

/*
** f1:
**	and	x0, x2, 1
**	ret
*/
/*
** f8:
**	and	x0, x2, 1
**	ret
*/
/*
** f16:
**	and	x0, x2, 1
**	ret
*/

/* fp seems to be unable to optimize away stack-usage, TODO: to fix.  */

/*
** fp:
**...
**	and	x0, x1, 1
**...
**	ret
*/

// all other f1p-f8p generate the same code, for f16p the value comes from x2
/*
** f1p:
**	and	x0, x1, 1
**	ret
*/
/*
** f8p:
**	and	x0, x1, 1
**	ret
*/
/*
** f16p:
**	and	x0, x2, 1
**	ret
*/

// g1-g16 are all the same
/*
** g1:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x4, \1, 9223372036854775807
**	and	x2, \1, 1
**	mov	x3, 0
**	b	f1
*/

/*
** g8:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x4, \1, 9223372036854775807
**	and	x2, \1, 1
**	mov	x3, 0
**	b	f8
*/
/*
** g16:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x4, \1, 9223372036854775807
**	and	x2, \1, 1
**	mov	x3, 0
**	b	f16
*/

// again gp different from the rest

/*
** gp:
**	sub	sp, sp, #16
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	sbfx	x([0-9]+), \1, 0, 63
**	mov	(w[0-9]+), 0
**	bfi	\3, w\2, 0, 1
**	and	x3, x\2, 9223372036854775807
**	mov	x2, 0
**	str	xzr, \[sp\]
**	strb	\3, \[sp\]
**	ldr	x1, \[sp\]
**	add	sp, sp, 16
**	b	fp
*/

// g1p-g8p are all the same, g16p uses x2 to pass parameter to f16p

/*
** g1p:
**	mov	(w[0-9]+), w1
**	and	x3, x0, 9223372036854775807
**	and	x1, x0, 1
**	mov	x2, 0
**	mov	w0, \1
**	b	f1p
*/
/*
** g8p:
**	mov	(w[0-9]+), w1
**	and	x3, x0, 9223372036854775807
**	and	x1, x0, 1
**	mov	x2, 0
**	mov	w0, \1
**	b	f8p
*/
/*
** g16p:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x4, \1, 9223372036854775807
**	and	x2, \1, 1
**	mov	x3, 0
**	b	f16p
*/

// f*_stack are all the same
/*
** f1_stack:
**	ldr	(x[0-9]+), \[sp, 16\]
**	and	x0, \1, 1
**	ret
*/
/*
** f8_stack:
**	ldr	(x[0-9]+), \[sp, 16\]
**	and	x0, \1, 1
**	ret
*/
/*
** f16_stack:
**	ldr	(x[0-9]+), \[sp, 16\]
**	and	x0, \1, 1
**	ret
*/

// fp{,1,8}_stack are all the same but fp16_stack loads from sp+16
/*
** fp_stack:
**	ldr	(x[0-9]+), \[sp, 8\]
**	and	x0, \1, 1
**	ret
*/
/*
** f1p_stack:
**	ldr	(x[0-9]+), \[sp, 8\]
**	and	x0, \1, 1
**	ret
*/
/*
** f8p_stack:
**	ldr	(x[0-9]+), \[sp, 8\]
**	and	x0, \1, 1
**	ret
*/

/*
** f16p_stack:
**	ldr	(x[0-9]+), \[sp, 16\]
**	and	x0, \1, 1
**	ret
*/

/*
** gp_stack:
**...
**	mov	x([0-9]+), x0
**	sxtw	(x[0-9]+), w1
**	mov	x0, \2
**	and	x7, \2, 9223372036854775807
**	mov	(w[0-9]+), 0
**	bfi	\3, w\1, 0, 1
**	strb	wzr, \[sp, 16\]
**	mov	x6, x7
**	mov	x5, x7
**	mov	x4, x7
**	mov	x3, x7
**	mov	x2, x7
**	str	xzr, \[sp, 48\]
**	strb	\3, \[sp, 48\]
**	ldr	(x[0-9]+), \[sp, 48\]
**	stp	x7, \4, \[sp\]
**	mov	x1, x7
**	bl	fp_stack
**	sbfx	x0, x0, 0, 63
**...
**	ret
*/

/*
** g1_stack:
**...
**	mov	(x[0-9]+), x0
**	sxtw	(x[0-9]+), w1
**	mov	x0, \2
**	and	x7, \2, 9223372036854775807
**	mov	(x[0-9]+), 0
**	sbfx	(x[0-9]+), \1, 0, 63
**	bfi	\3, \4, 0, 1
**	stp	\3, xzr, \[sp, 16\]
**	mov	x6, x7
**	mov	x5, x7
**	mov	x4, x7
**	mov	x3, x7
**	mov	x2, x7
**	mov	x1, x7
**	str	x7, \[sp\]
**	bl	f1_stack
**	sbfx	x0, x0, 0, 63
**...
**	ret
*/


/*
** g8_stack:
**...
**	mov	(x[0-9]+), x0
**	sxtw	(x[0-9]+), w1
**	mov	x0, \2
**	and	x7, \2, 9223372036854775807
**	mov	(x[0-9]+), 0
**	sbfx	(x[0-9]+), \1, 0, 63
**	bfi	\3, \4, 0, 1
**	stp	\3, xzr, \[sp, 16\]
**	mov	x6, x7
**	mov	x5, x7
**	mov	x4, x7
**	mov	x3, x7
**	mov	x2, x7
**	mov	x1, x7
**	str	x7, \[sp\]
**	bl	f8_stack
**	sbfx	x0, x0, 0, 63
**...
**	ret
*/

/*
** g16_stack:
**...
**	mov	(x[0-9]+), x0
**	sxtw	(x[0-9]+), w1
**	mov	x0, \2
**	and	(x[0-9]+), \2, 9223372036854775807
**	mov	(x[0-9]+), 0
**	sbfx	(x[0-9]+), \1, 0, 63
**	bfi	\4, \5, 0, 1
**	stp	\4, xzr, \[sp, 16\]
**	mov	x6, \3
**	mov	x5, \3
**	mov	x4, \3
**	mov	x3, \3
**	mov	x2, \3
**	mov	x1, \3
**	str	x7, \[sp\]
**	bl	f16_stack
**	sbfx	x0, x0, 0, 63
**...
**	ret
*/

/*
** f1_stdarg:
**...
**	and	x0, x2, 1
**...
**	ret
*/
/*
** f16_stdarg:
**...
**	and	x0, x2, 1
**...
**	ret
*/

/*
** fp_stdarg:
**...
**	and	x0, x1, 1
**...
**	ret
*/

/*
** f1p_stdarg:
**...
**	and	x0, x1, 1
**...
**	ret
*/
/*
** f8p_stdarg:
**...
**	and	x0, x1, 1
**...
**	ret
*/
/*
** f16p_stdarg:
**...
**	and	x0, x2, 1
**...
**	ret
*/

/*
** g1_stdarg:
**	and	x2, x0, 1
**	mov	x3, 0
**	mov	w0, w1
**	b	f1_stdarg
*/

/*
** g16_stdarg:
**	and	x2, x0, 1
**	mov	x3, 0
**	mov	w0, w1
**	b	f16_stdarg
*/

/*
** gp_stdarg:
**...
**	mov	x([0-9]+), x0
**	mov	w0, w1
**	mov	(w[0-9]+), 0
**	bfi	\2, w\1, 0, 1
**	mov	x2, 0
**	str	xzr, \[sp\]
**	strb	\2, \[sp\]
**	ldr	x1, \[sp\]
**...
**	b	fp_stdarg
*/

/*
** g1p_stdarg:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x1, \1, 1
**	mov	x2, 0
**	b	f1p_stdarg
*/

/*
** g8p_stdarg:
**	mov	(x[0-9]+), x0
**	mov	w0, w1
**	and	x1, \1, 1
**	mov	x2, 0
**	b	f8p_stdarg
*/

/*
** g16p_stdarg:
**	and	x2, x0, 1
**	mov	x3, 0
**	mov	w0, w1
**	b	f16p_stdarg
*/
