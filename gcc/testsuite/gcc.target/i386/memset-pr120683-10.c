/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -mmemset-strategy=unrolled_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { lp64 && { ! *-*-darwin* } } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { lp64 && *-*-darwin* } } {^\t?\.} } } */

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
*#

*Dfoo:
*D	movq	\$0, 29\(%rdi\)
*D	movq	\$0, 37\(%rdi\)
*D	movq	\$0, 45\(%rdi\)
*D	movq	\$0, 53\(%rdi\)
*D	movq	\$0, \(%rdi\)
*D	movq	\$0, 8\(%rdi\)
*D	movq	\$0, 16\(%rdi\)
*D	movq	\$0, 24\(%rdi\)
*D	ret
*D...
*E
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 61);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
