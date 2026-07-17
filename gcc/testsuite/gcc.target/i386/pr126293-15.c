/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=x86-64 -m128bit-atomic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movdqa	load\(%rip\), %xmm0
**	movaps	%xmm0, -24\(%rsp\)
**	movq	-24\(%rsp\), %rax
**	movq	-16\(%rsp\), %rdx
**	ret
**	.cfi_endproc
**...
*/

typedef struct
{
  int a[4];
} s;

_Atomic s load;

s
func (void)
{
  return load;
}
