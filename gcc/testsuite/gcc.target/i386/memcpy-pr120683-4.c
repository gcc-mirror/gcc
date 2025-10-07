/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%eax, %eax
**.L[0-9]+:
**	movl	%eax, %edx
**	subl	\$-128, %eax
**	vmovdqa	src\(%rdx\), %ymm3
**	vmovdqa	src\+32\(%rdx\), %ymm2
**	vmovdqa	src\+64\(%rdx\), %ymm1
**	vmovdqa	src\+96\(%rdx\), %ymm0
**	vmovdqa	%ymm3, dest\(%rdx\)
**	vmovdqa	%ymm2, dest\+32\(%rdx\)
**	vmovdqa	%ymm1, dest\+64\(%rdx\)
**	vmovdqa	%ymm0, dest\+96\(%rdx\)
**	cmpl	\$512, %eax
**	jb	.L[0-9]+
**	vmovdqa	src\(%rax\), %ymm0
**	vmovdqa	%ymm0, dest\(%rax\)
**	vzeroupper
**	ret
**...
*/

#define SIZE (16 + 1) * 32

char dest[SIZE];
char src[SIZE];

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
