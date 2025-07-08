/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=unrolled_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movabsq	\$289360691352306692, %rax
**	movq	%rax, 48\(%rdi\)
**	movq	%rax, \(%rdi\)
**	movq	%rax, 8\(%rdi\)
**	movq	%rax, 16\(%rdi\)
**	movq	%rax, 24\(%rdi\)
**	movq	%rax, 32\(%rdi\)
**	movq	%rax, 40\(%rdi\)
**	movq	%rax, 53\(%rdi\)
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 4, 61);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
