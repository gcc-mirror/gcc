/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv8-m.base" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-m23" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mfpu=*" } { } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=soft" } } */
/* { dg-options "-mcpu=cortex-m23 -mcmse" } */
/* { dg-additional-options "-Os" } */
/* { dg-final { check-function-bodies "**" "" } } */

int __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return 1;
}
/* { { dg-final { scan-assembler-not "mov\tr9, r0" } } */

/*
** __acle_se_bar:
**	mov	(r[0-3]), r9
**	push	{\1}
** ...
**	pop	{(r[0-3])}
**	mov	r9, \2
** ...
**	bxns	lr
*/
int __attribute__ ((cmse_nonsecure_entry))
bar (void)
{
  asm ("": : : "r9");
  return 1;
}
