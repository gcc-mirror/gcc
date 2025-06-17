/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-sse -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**...
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\$0, 49\(%rdi\)
**	xorl	%eax, %eax
**	movq	\$0, 57\(%rdi\)
**	movq	\$0, 65\(%rdi\)
**	movq	\$0, 73\(%rdi\)
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$32, %eax
**	movq	\$0, \(%rdi,%rdx\)
**	movq	\$0, 8\(%rdi,%rdx\)
**	movq	\$0, 16\(%rdi,%rdx\)
**	movq	\$0, 24\(%rdi,%rdx\)
**	cmpl	\$64, %eax
**	jb	.L[0-9]+
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 81);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
