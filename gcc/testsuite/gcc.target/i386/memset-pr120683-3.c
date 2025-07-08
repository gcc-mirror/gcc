/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	vpxor	%xmm0, %xmm0, %xmm0
**	vmovdqu8	%zmm0, 128\(%rdi\)
**	vmovdqu8	%zmm0, \(%rdi\)
**	vmovdqu8	%zmm0, 64\(%rdi\)
**	vmovdqu8	%zmm0, 190\(%rdi\)
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
