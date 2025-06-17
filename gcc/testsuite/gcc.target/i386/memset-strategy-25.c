/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-sse -fasynchronous-unwind-tables -fdwarf2-cfi-asm" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\$0, 221\(%rdi\)
**	xorl	%eax, %eax
**	movq	\$0, 229\(%rdi\)
**	movq	\$0, 237\(%rdi\)
**	movq	\$0, 245\(%rdi\)
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$32, %eax
**	movq	\$0, \(%rdi,%rdx\)
**	movq	\$0, 8\(%rdi,%rdx\)
**	movq	\$0, 16\(%rdi,%rdx\)
**	movq	\$0, 24\(%rdi,%rdx\)
**	cmpl	\$224, %eax
**	jb	.L[0-9]+
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 253);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
