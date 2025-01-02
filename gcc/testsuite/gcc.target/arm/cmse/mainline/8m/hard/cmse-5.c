/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "../../../cmse-5.x"

/*
** __acle_se_foo:
** ...
** (
**	mov	r0, lr
**	mov	r1, lr
**	mov	r2, lr
**	mov	r3, lr
** ...
**	vmov.f32	s1, #1\.0e\+0
**	vmov.f64	d1, #1\.0e\+0
**	vmov.f64	d2, #1\.0e\+0
**	vmov.f64	d3, #1\.0e\+0
**	vmov.f64	d4, #1\.0e\+0
**	vmov.f64	d5, #1\.0e\+0
**	vmov.f64	d6, #1\.0e\+0
**	vmov.f64	d7, #1\.0e\+0
** |
**	vmov.f32	s1, #1\.0e\+0
**	vmov.f64	d1, #1\.0e\+0
**	vmov.f64	d2, #1\.0e\+0
**	vmov.f64	d3, #1\.0e\+0
**	vmov.f64	d4, #1\.0e\+0
**	vmov.f64	d5, #1\.0e\+0
**	vmov.f64	d6, #1\.0e\+0
**	vmov.f64	d7, #1\.0e\+0
** ...
**	mov	r0, lr
**	mov	r1, lr
**	mov	r2, lr
**	mov	r3, lr
** )
**	msr	APSR_nzcvqg?, lr
**	push	(\{r4\})
**	vmrs	ip, fpscr
**	movw	r4, #65376
**	movt	r4, #4095
**	and	ip, r4
**	vmsr	fpscr, ip
**	pop	\1
**	mov	ip, lr
**	bxns	lr
*/

/* { dg-final { scan-assembler-not "vmov\.f64\td0, #1\.0e\+0" } } */
/* { dg-final { scan-assembler-not "vmov\.f32\ts0, #1\.0e\+0" } } */
