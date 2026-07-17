/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=x86-64 -m128bit-atomic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	fldt	8\(%rsp\)
**	fstpt	-24\(%rsp\)
**	movdqa	-24\(%rsp\), %xmm0
**	movaps	%xmm0, store\(%rip\)
**	lock orq	\$0, \(%rsp\)
**	ret
**	.cfi_endproc
**...
*/

extern _Atomic long double store;

void
func (long double i)
{
  store = i;
}
