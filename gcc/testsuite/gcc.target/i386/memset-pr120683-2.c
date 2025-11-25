/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	vpxor	%xmm0, %xmm0, %xmm0
**	vmovdqu	%ymm0, 192\(%rdi\)
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, 32\(%rdi\)
**	vmovdqu	%ymm0, 64\(%rdi\)
**	vmovdqu	%ymm0, 96\(%rdi\)
**	vmovdqu	%ymm0, 128\(%rdi\)
**	vmovdqu	%ymm0, 160\(%rdi\)
**	vmovdqu	%ymm0, 222\(%rdi\)
**	vzeroupper
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 254);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
