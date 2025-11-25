/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -mmemset-strategy=unrolled_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movabsq	\$72340172838076673, %rax
**	movzbl	%sil, %esi
**	imulq	%rax, %rsi
**	movq	%rsi, 48\(%rdi\)
**	movq	%rsi, \(%rdi\)
**	movq	%rsi, 8\(%rdi\)
**	movq	%rsi, 16\(%rdi\)
**	movq	%rsi, 24\(%rdi\)
**	movq	%rsi, 32\(%rdi\)
**	movq	%rsi, 40\(%rdi\)
**	movq	%rsi, 53\(%rdi\)
**	ret
**...
*/

void
foo (char *dest, int c)
{
  __builtin_memset (dest, c, 61);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
