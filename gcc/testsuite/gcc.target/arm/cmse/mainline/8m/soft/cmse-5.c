/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=soft" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=soft" } } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "../../../cmse-5.x"

/*
** __acle_se_foo:
**...
**	mov	r1, lr
**	mov	r2, lr
**	mov	r3, lr
**	mov	ip, lr
**	msr	APSR_nzcvqg?, lr
**	bxns	lr
*/

/* { dg-final { scan-assembler-not "mov\tr0, lr" } } */
