/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../union-4.x"

/*
** fn:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	mov	ip, r4
**	movw	r4, #7939
**	ands	r0, r4
**	mov	r4, ip
**	movs	r2, r4
**	movs	r3, r4
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
