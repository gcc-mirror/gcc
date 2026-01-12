/* { dg-do compile { target bitint } } */
/* { dg-additional-options "-march=rv64gc -mabi=lp64d -std=c23 -O -fno-stack-clash-protection -g" } */
/* { dg-skip-if "" { *-*-* } { "-flto" "-O0"} } */
/* { dg-final { check-function-bodies "**" "" } } */

#define CHECK_ARG(N)				\
void f##N(_BitInt(N) *ptr, _BitInt(N) y)	\
{						\
    *ptr = y;					\
}


CHECK_ARG(2)
/*
** f2:
**	sb	a1,0\(a0\)
**	ret
*/
CHECK_ARG(8)
/*
** f8:
**	sb	a1,0\(a0\)
**	ret
*/
CHECK_ARG(9)
/*
** f9:
**	sh	a1,0\(a0\)
**	ret
*/
CHECK_ARG(16)
/*
** f16:
**	sh	a1,0\(a0\)
**	ret
*/
CHECK_ARG(19)
/*
** f19:
**	sw	a1,0\(a0\)
**	ret
*/
CHECK_ARG(32)
/*
** f32:
**	sw	a1,0\(a0\)
**	ret
*/
CHECK_ARG(42)
/*
** f42:
**	sd	a1,0\(a0\)
**	ret
*/
CHECK_ARG(64)
/*
** f64:
**	sd	a1,0\(a0\)
**	ret
*/
CHECK_ARG(65)
/*
** f65:
**	sd	a1,0\(a0\)
**	sd	a2,8\(a0\)
**	ret
*/
CHECK_ARG(127)
/*
** f127:
**	sd	a1,0\(a0\)
**	sd	a2,8\(a0\)
**	ret
*/

CHECK_ARG(128)
/*
** f128:
**	sd	a1,0\(a0\)
**	sd	a2,8\(a0\)
**	ret
*/
