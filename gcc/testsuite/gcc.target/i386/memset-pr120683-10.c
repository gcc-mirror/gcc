/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=unrolled_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\$0, 48\(%rdi\)
**	movq	\$0, \(%rdi\)
**	movq	\$0, 8\(%rdi\)
**	movq	\$0, 16\(%rdi\)
**	movq	\$0, 24\(%rdi\)
**	movq	\$0, 32\(%rdi\)
**	movq	\$0, 40\(%rdi\)
**	movq	\$0, 53\(%rdi\)
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 61);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
