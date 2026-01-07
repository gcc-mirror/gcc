/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../../../union-4.x"

/*
** fn:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	movw	ip, #7939
**	and	r0, r0, ip
**	mov	r2, r4
**	mov	r3, r4
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
