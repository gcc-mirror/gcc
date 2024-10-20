/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-sp-d16" }  */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "Skip these if testing double precision" {*-*-*} {"-mfpu=fpv[4-5]-d16"} {""} } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "../../../cmse-5.x"

/*
** __acle_se_foo:
**...
** (
**	mov	r0, lr
**	mov	r1, lr
**	mov	r2, lr
**	mov	r3, lr
** ...
**	vmov.f32	s1, #1\.0e\+0
**	vmov.f32	s2, #1\.0e\+0
**	vmov.f32	s3, #1\.0e\+0
**	vmov.f32	s4, #1\.0e\+0
**	vmov.f32	s5, #1\.0e\+0
**	vmov.f32	s6, #1\.0e\+0
**	vmov.f32	s7, #1\.0e\+0
**	vmov.f32	s8, #1\.0e\+0
**	vmov.f32	s9, #1\.0e\+0
**	vmov.f32	s10, #1\.0e\+0
**	vmov.f32	s11, #1\.0e\+0
**	vmov.f32	s12, #1\.0e\+0
**	vmov.f32	s13, #1\.0e\+0
**	vmov.f32	s14, #1\.0e\+0
**	vmov.f32	s15, #1\.0e\+0
** |
**	vmov.f32	s1, #1\.0e\+0
**	vmov.f32	s2, #1\.0e\+0
**	vmov.f32	s3, #1\.0e\+0
**	vmov.f32	s4, #1\.0e\+0
**	vmov.f32	s5, #1\.0e\+0
**	vmov.f32	s6, #1\.0e\+0
**	vmov.f32	s7, #1\.0e\+0
**	vmov.f32	s8, #1\.0e\+0
**	vmov.f32	s9, #1\.0e\+0
**	vmov.f32	s10, #1\.0e\+0
**	vmov.f32	s11, #1\.0e\+0
**	vmov.f32	s12, #1\.0e\+0
**	vmov.f32	s13, #1\.0e\+0
**	vmov.f32	s14, #1\.0e\+0
**	vmov.f32	s15, #1\.0e\+0
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

/* { dg-final { scan-assembler-not "vmov\.f32\ts0, #1\.0e\+0" } } */
