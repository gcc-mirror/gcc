/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movd	%edi, %xmm0
**	movb	%dil, dest\+176\(%rip\)
**	punpcklbw	%xmm0, %xmm0
**	punpcklwd	%xmm0, %xmm0
**	pshufd	\$0, %xmm0, %xmm0
**	movaps	%xmm0, dest\(%rip\)
**	movaps	%xmm0, dest\+16\(%rip\)
**	movaps	%xmm0, dest\+32\(%rip\)
**	movaps	%xmm0, dest\+48\(%rip\)
**	movaps	%xmm0, dest\+64\(%rip\)
**	movaps	%xmm0, dest\+80\(%rip\)
**	movaps	%xmm0, dest\+96\(%rip\)
**	movaps	%xmm0, dest\+112\(%rip\)
**	movaps	%xmm0, dest\+128\(%rip\)
**	movaps	%xmm0, dest\+144\(%rip\)
**	movaps	%xmm0, dest\+160\(%rip\)
**	ret
**...
*/

char dest[177];

void
foo (int c)
{
  __builtin_memset (dest, c, sizeof (dest));
}

/* { dg-final { scan-assembler-not "rep stos" } } */
