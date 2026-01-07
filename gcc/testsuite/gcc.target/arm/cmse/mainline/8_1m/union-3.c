
/* { dg-do compile } */
/* { dg-options "-mcmse" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "../../union-3.x"

/*
** fn_caller_0:
** ...
**	lsrs	r3, r3, #1
**	lsls	r3, r3, #1
**	push	{r4, r5, r6, r7, r8, r9, r10, fp}
This test is not meant to be to test the FP clearing, so accept any, preventing
the need to specialize the test further for different float-abi's
** ...
**	clrm	{r2, r4, r5, r6, r7, r8, r9, r10, fp, ip, APSR}
**	blxns	r3
** ...
*/

/*
** fn_caller_1:
** ...
**	lsrs	r3, r3, #1
**	lsls	r3, r3, #1
**	push	{r4, r5, r6, r7, r8, r9, r10, fp}
** ...
**	clrm	{r1, r2, r4, r5, r6, r7, r8, r9, r10, fp, ip, APSR}
**	blxns	r3
** ...
*/
