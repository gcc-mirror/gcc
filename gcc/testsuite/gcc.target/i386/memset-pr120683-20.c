/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "*#" "" { target { lp64 && { ! *-*-darwin* } } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { lp64 && *-*-darwin* } } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	movd	%edi, %xmm0
**	punpcklbw	%xmm0, %xmm0
**	punpcklwd	%xmm0, %xmm0
**	pshufd	\$0, %xmm0, %xmm0
**	movaps	%xmm0, dest\+160\(%rip\)
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
**	movd	%xmm0, dest\+175\(%rip\)
**	ret
**...
*#

*Dfoo:
*D	movd	%edi, %xmm0
*D	punpcklbw	%xmm0, %xmm0
*D	punpcklwd	%xmm0, %xmm0
*D	pshufd	\$0, %xmm0, %xmm0
*D	movaps	%xmm0, 160\+_dest\(%rip\)
*D	movaps	%xmm0, _dest\(%rip\)
*D	movaps	%xmm0, 16\+_dest\(%rip\)
*D	movaps	%xmm0, 32\+_dest\(%rip\)
*D	movaps	%xmm0, 48\+_dest\(%rip\)
*D	movaps	%xmm0, 64\+_dest\(%rip\)
*D	movaps	%xmm0, 80\+_dest\(%rip\)
*D	movaps	%xmm0, 96\+_dest\(%rip\)
*D	movaps	%xmm0, 112\+_dest\(%rip\)
*D	movaps	%xmm0, 128\+_dest\(%rip\)
*D	movaps	%xmm0, 144\+_dest\(%rip\)
*D	movd	%xmm0, 175\+_dest\(%rip\)
*D	ret
*D...
*E
*/

char dest[179];

void
foo (int c)
{
  __builtin_memset (dest, c, sizeof (dest));
}

/* { dg-final { scan-assembler-not "rep stos" } } */
