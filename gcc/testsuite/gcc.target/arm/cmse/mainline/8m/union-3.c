
/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../../union-3.x"

/*
** fn_caller_0:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	mov	r2, r4
**	mov	r3, r4
This test is not meant to be to test the FP clearing, so accept any, preventing
the need to specialize the test further for different float-abi's
** ...
**	bl	__gnu_cmse_nonsecure_call
** ...
*/

/*
** fn_caller_1:
** ...
**	lsrs	r4, r4, #1
**	lsls	r4, r4, #1
**	mov	r1, r4
**	mov	r2, r4
**	mov	r3, r4
** ...
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
