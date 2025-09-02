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
**	st.b	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(8)
/*
** f8:
**	st.b	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(9)
/*
** f9:
**	st.h	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(16)
/*
** f16:
**	st.h	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(19)
/*
** f19:
**	stptr.w	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(32)
/*
** f32:
**	stptr.w	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(42)
/*
** f42:
**	stptr.d	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(64)
/*
** f64:
**	stptr.d	\$r5,\$r4,0
**	jr	\$r1
*/
CHECK_ARG(65)
/*
** f65:
**	stptr.d	\$r5,\$r4,0
**	st.d	\$r6,\$r4,8
**	jr	\$r1
*/
CHECK_ARG(127)
/*
** f127:
**	stptr.d	\$r5,\$r4,0
**	st.d	\$r6,\$r4,8
**	jr	\$r1
*/

CHECK_ARG(128)
/*
** f128:
**	stptr.d	\$r5,\$r4,0
**	st.d	\$r6,\$r4,8
**	jr	\$r1
*/
