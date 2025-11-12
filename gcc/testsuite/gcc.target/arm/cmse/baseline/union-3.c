
/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../union-3.x"

/*
** fn_caller_0:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	movs	r2, r4
**	movs	r3, r4
**	bl	__gnu_cmse_nonsecure_call
** ...
*/

/*
** fn_caller_1:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	movs	r1, r4
**	movs	r2, r4
**	movs	r3, r4
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
