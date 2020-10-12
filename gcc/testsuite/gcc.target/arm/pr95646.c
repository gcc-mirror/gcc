/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_base_ok } */
/* { dg-add-options arm_arch_v8m_base } */
/* { dg-additional-options "-mcmse -Os" } */
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
  __asm__ ("" : : : "r9");
  return 1;
}
