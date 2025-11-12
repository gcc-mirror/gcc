/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
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
**	vmov.f64	d0, #1.0e\+0
**	vmov.f64	d1, #1.0e\+0
**	vmov.f64	d2, #1.0e\+0
**	vmov.f64	d3, #1.0e\+0
**	vmov.f64	d4, #1.0e\+0
**	vmov.f64	d5, #1.0e\+0
**	vmov.f64	d6, #1.0e\+0
**	vmov.f64	d7, #1.0e\+0
**	bl	__gnu_cmse_nonsecure_call
** ...
*/
