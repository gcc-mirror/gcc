/* { dg-do compile { target bitint } } */
/* { dg-additional-options "-std=c23 -O -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#define CHECK_ARG(N)				\
void f##N(_BitInt(N) *ptr, _BitInt(N) y)	\
{						\
    *ptr = y;					\
}


CHECK_ARG(2)
/*
** f2:
**	sbfiz	(w[0-9]+), w1, 6, 2
**	asr	(w[0-9]+), \1, 6
**	strb	\2, \[x0\]
**	ret
*/
CHECK_ARG(8)
/*
** f8:
**	strb	w1, \[x0\]
**	ret
*/
CHECK_ARG(9)
/*
** f9:
**	sbfiz	(w[0-9]+), w1, 7, 9
**	asr	(w[0-9]+), \1, 7
**	strh	\2, \[x0\]
**	ret
*/
CHECK_ARG(16)
/*
** f16:
**	strh	w1, \[x0\]
**	ret
*/
CHECK_ARG(19)
/*
** f19:
**	sbfx	x([0-9]+), x1, 0, 19
**	str	w\1, \[x0\]
**	ret
*/
CHECK_ARG(32)
/*
** f32:
**	str	w1, \[x0\]
**	ret
*/
CHECK_ARG(42)
/*
** f42:
**	sbfx	(x[0-9]+), x1, 0, 42
**	str	\1, \[x0\]
**	ret
*/
CHECK_ARG(64)
/*
** f64:
**	str	x1, \[x0\]
**	ret
*/

CHECK_ARG(65)
/*
** f65:
**	extr	(x[0-9]+), x3, x2, 1
**	and	(x[0-9]+), x2, 1
**	orr	(x[0-9]+), \2, \1, lsl 1
**	asr	(x[0-9]+), \1, 63
**	stp	\3, \4, \[x0\]
**	ret
*/

CHECK_ARG(127)
/*
** f127:
**	extr	(x[0-9]+), x3, x2, 63
**	and	(x[0-9]+), x2, 9223372036854775807
**	orr	(x[0-9]+), \2, \1, lsl 63
**	asr	(x[0-9]+), \1, 1
**	stp	\3, \4, \[x0\]
**	ret
*/

CHECK_ARG(128)
/*
** f128:
**	stp	x2, x3, \[x0\]
**	ret
*/

CHECK_ARG(129)
/*
** f129:
**	ldp	(x[0-9]+), (x[0-9]+), \[x1\]
**	stp	\1, \2, \[x0\]
**	ldr	(x[0-9]+), \[x1, 16\]
**	sbfx	(x[0-9]+), \3, 0, 1
**	str	\4, \[x0, 16\]
**	ret
*/
